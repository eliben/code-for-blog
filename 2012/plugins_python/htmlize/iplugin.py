#-------------------------------------------------------------------------------
# htmlize: htmlize/iplugin.py
#
# The plugin interface. Plugins that want to register with htmlize must inherit
# IPlugin.
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
#-------------------------------------------------------------------------------
import imp
import os


class IPluginRegistry(type):
    plugins = []
    def __init__(cls, name, bases, attrs):
        if name != 'IPlugin':
            IPluginRegistry.plugins.append(cls)


class IPlugin(object, metaclass=IPluginRegistry):
    def __init__(self, post=None, db=None):
        """ Initialize the plugin. Optinally provide the db.Post that is
            being processed and the db.DB it belongs to.
        """
        self.post = post
        self.db = db

    """ Plugin classes inherit from IPlugin. The methods below can be
        implemented to provide services.
    """
    def get_role_hook(self, role_name):
        """ Return a function accepting role contents.
            The function will be called with a single argument - the role
            contents, and should return what the role gets replaced with.
            None if the plugin doesn't provide a hook for this role.
        """
        return None

    def get_contents_hook(self):
        """ Return a function accepting full document contents.
            The functin will be called with a single argument - the document
            contents (after paragraph splitting and role processing), and
            should return the transformed contents.
            None if the plugin doesn't provide a hook for this role.
        """
        return None


def discover_plugins(dirs):
    """ Discover the plugin classes contained in Python files, given a
        list of directory names to scan. Return a list of plugin classes.
    """
    for dir in dirs:
        for filename in os.listdir(dir):
            modname, ext = os.path.splitext(filename)
            if ext == '.py':
                file, path, descr = imp.find_module(modname, [dir])
                if file:
                    # Loading the module registers the plugin in
                    # IPluginRegistry
                    mod = imp.load_module(modname, file, path, descr)
    return IPluginRegistry.plugins


