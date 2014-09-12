#include "plugin_manager.h"
#include "clib/mem.h"


// The hooks are contained in simple singly-linked lists
//

typedef struct PluginRoleHookList_t {
    // The role for which this hook is registered, and the hook itself
    dstring role;
    PluginRoleHook hook;
    struct PluginRoleHookList_t* next;
} PluginRoleHookList;


typedef struct PluginContentsHookList_t {
    PluginContentsHook hook;
    struct PluginContentsHookList_t* next;
} PluginContentsHookList;


struct PluginManager_t {
    PluginRoleHookList* role_hook_list;
    PluginContentsHookList* contents_hook_list;
};


PluginManager* PluginManager_new() {
    PluginManager* pm = mem_alloc(sizeof(*pm));
    pm->role_hook_list = NULL;
    pm->contents_hook_list = NULL;
    return pm;
}


void PluginManager_free(PluginManager* pm) {
    PluginRoleHookList* role_plugin = pm->role_hook_list;
    while (role_plugin) {
        PluginRoleHookList* next = role_plugin->next;
        dstring_free(role_plugin->role);
        mem_free(role_plugin);
        role_plugin = next;
    }

    PluginContentsHookList* contents_plugin = pm->contents_hook_list;
    while (contents_plugin) {
        PluginContentsHookList* next = contents_plugin->next;
        mem_free(contents_plugin);
        contents_plugin = next;
    }
    mem_free(pm);
}


void PluginManager_register_role_hook(PluginManager* pm, dstring rolename,
                                      PluginRoleHook hook) {
    PluginRoleHookList* node = mem_alloc(sizeof(*node));
    node->role = dstring_dup(rolename);
    node->hook = hook;
    node->next = pm->role_hook_list;
    pm->role_hook_list = node;
}


void PluginManager_register_contents_hook(PluginManager* pm,
                                          PluginContentsHook hook) {
    PluginContentsHookList* node = mem_alloc(sizeof(*node));
    node->hook = hook;
    node->next = pm->contents_hook_list;
    pm->contents_hook_list = node;
}


dstring PluginManager_apply_role_hooks(PluginManager* pm, dstring rolename,
                                       dstring rolecontents,
                                       DB* db, Post* post) {
    PluginRoleHookList* role_plugin = pm->role_hook_list;
    while (role_plugin) {
        if (dstring_compare(rolename, role_plugin->role) == 0) {
            return role_plugin->hook(rolecontents, db, post);
        }

        role_plugin = role_plugin->next;
    }

    return NULL;
}


dstring PluginManager_apply_contents_hooks(PluginManager* pm, dstring contents,
                                           DB* db, Post* post) {
    PluginContentsHookList* contents_plugin = pm->contents_hook_list;
    if (!contents_plugin)
        return NULL;

    contents = dstring_dup(contents);
    while (contents_plugin) {
        dstring new_contents = contents_plugin->hook(contents, db, post);
        dstring_free(contents);
        contents = new_contents;

        contents_plugin = contents_plugin->next;
    }

    return contents;
}

