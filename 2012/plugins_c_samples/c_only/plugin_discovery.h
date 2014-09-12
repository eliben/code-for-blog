//
// plugin_discovery.h: Plugin discovery interface.
//
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#ifndef PLUGIN_DISCOVERY_H
#define PLUGIN_DISCOVERY_H

#include "clib/dstring.h"
#include "plugin_manager.h"


// Given a directory name, looks for plugins in this directory. pm
// is passed to plugins for registration.
// Returns an opaque state object that has to be passed to the cleanup function
// later. If no plugins are found, returns NULL.
//
void* discover_plugins(dstring dirname, PluginManager* pm);

// Cleans up the plugins's resources. Must be called to release plugins in
// order to avoid resource leaks. Accepts the opaque object returned from
// discover_plugins.
//
void cleanup_plugins(void*);


#endif /* PLUGIN_DISCOVERY_H */

