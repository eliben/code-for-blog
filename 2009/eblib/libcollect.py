""" 
libcollect.py

Provides the LibCollect class, used for collecting the various libraries
your script uses for delivery as a self-contained distribution package.

Author: Eli Bendersky (http://eli.thegreenplace.net)
License: Same as Python

Motivation:

Imagine that you've written a script that uses several libraries, some of 
which you've written and some you've downloaded and installed (for example 
PyYAML). You want to distribute the script to your friends and co-workers, 
who already have Python installed with all the standard library. But your 
script won't run on their machines, because they have neither your personal
libraries, nor PyYAML installed. So what can you do ?
* You can ask them to install PyYAML and other libraries your script uses,
  and send them your own libraries. This is a lengthy and inconvenient 
  process.
* You can use a tool like py2exe to package your delivery. This has a 
  downside, however. py2exe produces large files (several MBs) and you may
  not want that.
* You can painstakingly collect the libraries into a directory where your
  script can find them, and package the directory together with the script.

LibCollect makes the third option trivial, by doing all the dirty work
for you. 

Example:

Suppose your script is named script.py, and is located in directory $DIR
(although I'm using Unix-y notation here, it is for convenience only. 
LibCollect works similarly well on Windows platforms). Follow these steps
to prepare a self-contained distribution with LibCollect:

Create a distribution setup script in the same directory. Lets assume 
you call it distrib_script.py. You can easily place it in any directory
you like, I'm using the same one to make the example simpler.

Add the following to distrib_script.py (assuming that libcollect.py is
in your sys.path):

**************************************************************

import libcollect

# Create a LibCollect object
lc = libcollect.LibCollect()

# Prepare arguments for do_collect
#

# Path to the script (can be absolute or relative)
scriptname = 'script.py'

# Ask the resulting distribution to be placed in
# directory distrib
targetdir = 'distrib'

# Specify which libraries to exclude from the 
# distribution (because you know they're installed
# on the target machine)
excludes = ["wx",
            "pywin",
            "win32api",
            "win32com"]

# This does the actual work
# See the documentation of LibCollect for more options
#
lc.do_collect(  scriptname, 
                targetdir, 
                excludes,
                verbose=True)

**************************************************************

Now run distrib_script.py.
When it finishes running, you will see that the distrib directory
has been created in $DIR. 
In $DIR/distrib you will see a file: script.py and 
a directory: lib

* script.py is a loader that replaces your original script.py - this
  is the program your users should run. All it does (look at the 
  code, it's short!) is prepare the sys.path to include the 
  packaged libraries, and runs your own script.py that was also 
  packaged into the .zip file
* lib is the distribution library, containing all the code
  your script needs to run on any machine with Python installed,
  and nothing else (except the modules you specified in the exclusion
  list).

How to use LibCollect:

* It is most convenient to use LibCollect in the way demonstrated
  in the example above. You may want to update your application from
  time to time, and having a distribution script handy will turn 
  the preparation of a new distribution into a 5-second process.
* If you don't want to create a distribution script, you can use
  a more direct method of invoking libcollect.py as a program on 
  your script. Call it without arguments and it will print
  a usage string that will explain what you need to do.

How it works:

* LibCollect uses the standard modulefinder module to find out which
  libraries are used by your script. It categorizes them into two
  types: standard libraries that came with Python, and non-standard 
  libraries you've installed or written.
* Only libraries of the second type are included in the distribution
  (bar the libraries you've explicitly asked to exclude).
* It then builds a directory with all the included libraries, in a 
  way that your script will be able to find them. The script itself
  is also packaged into the same place.
* On request, this directory can be zipped into a single file, to
  employ Python's built-in zipimport facility (but read the docstring 
  of the LibCollect class for more information on this)
* In the distribution directory, a new file with the name of your
  script is created. It is a simple loader that uses the runpy module
  to transparently load your script from the distribution library.
  This way your script is not being modified (sys.path is rigged
  from the loader).

Compatibility:
    Written in pure Python 2.5
    Tested on Windows and Linux, but should work on other platforms
    where the standard Python distribution works.

Version history:

    1.0  (2008.06.07): initial release
    1.1  (2008.07.03): create an unzipped distribution library
                       by default, because of the limitations 
                       of zipimport.
"""

from distutils.archive_util import make_zipfile
from distutils.dir_util import mkpath, create_tree
import distutils.sysconfig
import os, sys
import shutil
from modulefinder import ModuleFinder

version = "1.1"


class LibCollect(object):
    """ See module documentation for an introduction and example.
    
        Usage:
        
            lc = LibCollect()
            lc.do_collect(...)
        
        The documentation of do_collect provides the gory details.
    """
    def __init__(self):
        pass
            
    def do_collect(self, scriptname, targetdir, excludes=[], distlib='lib', zip_lib=False, verbose=False):
        """ See module documentation for an introduction and example.
        
            do_collect performs the actual work of this module.
            
            Arguments:
            
            scriptname  Path to your top-level application file. Can be
                        either relative or absolute.
            
            targetdir   Path to the target directory where the packaged
                        distribution will be placed. The distribution
                        consists of a loader script and a distribution
                        library (either a directory or a zip file).
                        This directory may not exist prior to invocation. 
                        If it exists, it will be overridden.
                        
            excludes    A list of module names for exclusion from the
                        distribution. For example, if you know all your
                        users have wxPython installed, you may want to
                        add 'wx' to this list - it will save a lot of 
                        space.
            
            distlib     Name of the distribution library that will be
                        created in targetdir. 
                        
            zip_lib     True if you want the distribution library to be 
                        zipped into a single file. False if you want it 
                        to be an uncompressed directory.
                        
                        Notes: 
                        
                        * While saving disk space, this option is likely
                          to hinder the start-up performance of the 
                          script, because Python won't pre-compile the
                          .py files into .pyc files after the first load
                          if the .py files are in a zip archive.
                        * Due to a limitation of zipimport (Python's
                          built-in importer from zip files), your script
                          won't work after distribution if the library
                          contains extensions (.pyd & .pyo) or 
                          console-less Windows scripts (.pyw). See the 
                          documentation of zipimport for more information.
                                                
            verbose     True to make do_collect print out its progress
                        to stdout. May be useful for the first time you
                        create a distribution for some application.
            
            Returns:
            
            Nothing. An exception may be thrown with an error message from
            one of the undelying method calls.
        """
        self.site_packages = os.path.normcase(distutils.sysconfig.get_python_lib(standard_lib=False))
        self.standard_lib = os.path.normcase(distutils.sysconfig.get_python_lib(standard_lib=True))
        self.sys_prefix = os.path.normcase(sys.prefix)
        
        self.verbose = verbose
        self.log("\nLibCollect v%s running in verbose mode\n" % version)
        
        # Initial preparation to create the lib directory
        #
        if os.path.exists(targetdir): 
            self.log("Directory '%s' exists. Removing it." % targetdir)
            shutil.rmtree(targetdir)
            
        libdir = os.path.join(targetdir, distlib)
        self.log("Creating path '%s'" % libdir)
        mkpath(libdir)
        
        # Find the modules we need to collect
        # 
        modules = self.find_modules(scriptname, excludes, verbose)
    
        self.log("Collecting modules into '%s'" % libdir)
        # Collect the modules in the lib directory
        #
        for modname, modtype, modfile in modules:
            modname_components = modname.split('.')
            
            if modtype == 'm':
                if len(modname_components) > 1:
                    new_path = os.path.join(libdir, *modname_components[0:-1])
                else:
                    new_path = libdir
            elif modtype == 'P':
                new_path = os.path.join(libdir, *modname_components)
            else:
                assert False
            
            mkpath(new_path)
            shutil.copy(modfile, new_path)
        
        os.chdir(targetdir)
        
        if zip_lib:
            self.log("Zipping directory '%s' into '%s'" % (libdir, libdir + '.zip'))
            make_zipfile(distlib, distlib)
            self.log("Removing directory '%s'" % libdir)
            shutil.rmtree(distlib)
            path_add = "os.path.join('" + distlib + ".zip', '" + distlib + "')"
        else:
            path_add = "'" + distlib + "'"
        
        # Create the loader script
        #
        self.log("Writing loader script: %s" % scriptname)
        loader = open(os.path.basename(scriptname), 'w')
        loader_name = os.path.splitext(scriptname)[0]
        loader.write("import os, sys, runpy\n")
        loader.write("sys.path.insert(0, %s)\n" % path_add)
        loader.write("runpy.run_module('%s', run_name=\"__main__\", alter_sys=True)\n" % loader_name)
        loader.close()
            
    def find_modules(self, scriptname, excludes=[], verbose=False):
        """ Find the modules we'd want to include in the 
            distribution.
        """
        path = sys.path[:]
        path.insert(0, os.path.dirname(scriptname))
        
        mf = ModuleFinder(path=path, excludes=excludes)
        mf.run_script(scriptname)
        
        modulenames = mf.modules.keys()
        modulenames.sort()
        
        self.log("Looking for modules used by '%s'...\n" % scriptname)
        log_format = "%-2s %-30s %s"
        self.log(log_format % ('', 'Module name', 'Module location'))
        self.log(log_format % ('--', '-' * 30, '-' * 30))
        modules = []
        
        for name in modulenames: 
            m = mf.modules[name]
            
            # builtin
            #
            if not m.__file__: continue 
            
            mpath = os.path.normcase(m.__file__)
            
            # Modules in Python distribution.
            # Pass on only those that live in site-packages
            #
            if mpath.startswith(self.site_packages): 
                pass
            elif mpath.startswith(self.sys_prefix):
                continue
            
            type = "P" if m.__path__ else "m"
            modules.append((name, type, m.__file__))
            
            self.log(log_format % (type, name, m.__file__))

        self.log("")
        return modules
    
    def log(self, msg):
        if self.verbose: print msg


if __name__ == "__main__":    
    from optparse import OptionParser
    
    usage = "usage: %prog [options] script"
    description = "Collect the script with the libraries it uses into a distribution. See module documentation for more details"
    
    opts = OptionParser(usage=usage, description=description)
    
    #~ opts.add_option("-h", "--help", action="help")
    opts.add_option('-t', '--targetdir', dest='targetdir', 
                    help='place distribution into TARGETDIR')
    opts.add_option('-z', '--zip_lib', dest='zip_lib', action='store_true',
                    help='zip the distribution library')
    opts.add_option('-v', '--verbose', dest='verbose', action='store_true',
                    help='print progress')
    opts.add_option('-e', '--exclude', dest='excludes', action='append',
                    help='exclude library from distribution. You can provide several of thsese')
    
    opts.set_defaults(  targetdir='distrib',
                        zip_lib=True,
                        excludes=[],
                        verbose=False)
    
    (options, args) = opts.parse_args()

    if len(args) != 1: 
        opts.print_help()
        sys.exit(0)

    lc = LibCollect()
    lc.do_collect(  args[0], 
                    options.targetdir, 
                    options.excludes, 
                    distlib='lib', 
                    verbose=options.verbose, 
                    zip_lib=options.zip_lib)
