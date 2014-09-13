from eblib import libcollect

# Create a LibCollect object
lc = libcollect.LibCollect()

# Prepare arguments for do_collect
#

# Path to the script (can be absolute or relative)
scriptname = 'plotting_data_monitor.pyw'

# Ask the resulting distribution to be placed in
# directory distrib
targetdir = 'distrib'

# Specify which libraries to exclude from the 
# distribution (because you know they're installed
# on the target machine)
excludes = ["PyQt4",
            "numpy",
            "serial",
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
