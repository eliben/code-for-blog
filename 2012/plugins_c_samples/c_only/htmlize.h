//
// htmlize.h: Core htmlize interface
// 
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#ifndef HTMLIZE_H
#define HTMLIZE_H

#include "clib/dstring.h"
#include "db.h"
#include "plugin_manager.h"


// "HTMLize" a given post. The post's contents are taken and converted to
// HTML. Calls into plugins (represented by PluginManager) if applicable.
// Returns a HTML string.
//
dstring htmlize(PluginManager* pm, DB* db, Post* post);

#endif /* HTMLIZE_H */

