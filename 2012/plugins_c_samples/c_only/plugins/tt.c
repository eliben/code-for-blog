//
// plugins/tt.c: <tt> plugin. Looks for :tt:`text` roles, and converts
// them to <tt>text</tt>.
//
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#include "clib/dstring.h"
#include "db.h"
#include "plugin_manager.h"


static dstring tt_role_hook(dstring str, DB* db, Post* post) {
    return dstring_format("<tt>%s</tt>", dstring_cstr(str));
}


int init_tt(PluginManager* pm) {
    dstring rolename = dstring_new("tt");
    PluginManager_register_role_hook(pm, rolename, tt_role_hook);
    dstring_free(rolename);
    return 1;
}

