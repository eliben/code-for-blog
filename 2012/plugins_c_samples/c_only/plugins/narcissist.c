//
// plugins/narcissist.c: The narcissist plugin. Looks for instances of "I",
// adds a username in parens and makes it all bold, i.e.:
//
// I --> <b>I (john.smith)</b>
//
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#include "clib/dstring.h"
#include "db.h"
#include "plugin_manager.h"
#include <ctype.h>
#include <string.h>


static inline int isseparator(char c) {
    return isspace(c) || ispunct(c);
}


static dstring narcissist_contents_hook(dstring str, DB* db, Post* post) {
    dstring replacement = dstring_format("<b>I (%s)</b>",
                            dstring_cstr(Post_get_author(post)));
    str = dstring_dup(str);

    // In the main loop, p points to the current position in the string.
    char* p = dstring_cstr(str);

    while (1) {
        char* nextI = strchr(p, 'I');
        if (!nextI)
            break;

        // Found "I". Make sure it's really a stand-alone "I", not part of
        // some longer word.
        if (isseparator(*(nextI + 1)) && isseparator(*(nextI - 1))) {
            size_t len_before_I = nextI - dstring_cstr(str);
            dstring newstr = dstring_new_len(dstring_cstr(str),
                                             len_before_I);
            dstring_concat(newstr, replacement);
            dstring_concat_cstr(newstr, nextI + 1);

            dstring_free(str);
            str = newstr;
            p = dstring_cstr(str) + len_before_I + dstring_len(replacement);
        } else {
            p = nextI + 1;
        }
    }

    dstring_free(replacement);
    return str;
}


int init_narcissist(PluginManager* pm) {
    PluginManager_register_contents_hook(pm, narcissist_contents_hook);
    return 1;
}


