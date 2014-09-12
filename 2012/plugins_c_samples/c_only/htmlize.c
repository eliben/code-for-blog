//
// htmlize.c: Core htmlize functionality
// 
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#include "htmlize.h"

#include <ctype.h>
#include <string.h>
#include <stdio.h>


// Inserts <p>...</p> marks around paragraphs of text. Returns the
// new contents.
//
static dstring insert_paragraphs(dstring contents) {
    dstring result = dstring_empty();

    // Note: this is outrageously inefficient - result keeps getting
    // reallocated. What matters for this sample is simplicity, though.
    char* p = dstring_cstr(contents);
    char* next_break = p;

    while (next_break && *next_break) {
        // Loop invariant: p points at the beginning of the current
        // paragraph.
        next_break = strstr(p, "\n\n");

        // Now next_break either points at the end of the paragraph, or
        // is NULL which means that the current paragraph extends until the
        // end of the string.
        dstring paragraph;
        if (next_break) {
            size_t len = next_break - p;
            paragraph = dstring_new_len(p, len);
        } else {
            paragraph = dstring_new(p);
        }

        // Replace all newlines with spaces inside the paragraph
        dstring_replace_char(paragraph, '\n', ' ');

        // Prepare the delimited paragraph and concat it to the result.
        dstring delimited_paragraph = dstring_format("<p>%s</p>\n\n",
                                                     dstring_cstr(paragraph));
        dstring_concat(result, delimited_paragraph);
        dstring_free(paragraph);
        dstring_free(delimited_paragraph);

        // Roll next_break until it points to something that is not \n
        while (next_break && *next_break == '\n') {
            next_break++;
        }

        p = next_break;
    }

    return result;
}


// Applies the contents hooks from pm to the contents. Always returns a new
// dstring, even if it was not modified by plugins.
//
static dstring apply_content_hooks(dstring contents, PluginManager* pm,
                                   DB* db, Post* post) {
    dstring newcontents = PluginManager_apply_contents_hooks(pm, contents,
                                                             db, post);
    return newcontents ? newcontents : dstring_dup(contents);
}


// Applies the role hooks from pm to the contents. Always returns a new
// dstring, even if it was not modified by plugins.
//
static dstring apply_role_hooks(dstring contents, PluginManager* pm,
                                DB* db, Post* post) {
    // text holds the contents - it is changed to point to another object
    // when a new contents is created.
    dstring text = dstring_dup(contents);
    // In the main loop, p points at the "current" position in the string.
    char* p = dstring_cstr(text);

    while (1) {
        // Look for the next role candidate
        char* startcolon = strchr(p, ':');
        if (!startcolon)
            break;
        // We saw a colon. It must be followed by alphanumeric chars and
        // another colon, and then a backtick.
        p = startcolon + 1;
        while (isalpha(*p))
            p++;
        if (*p != ':')
            break;
        char* endcolon = p;
        char* starttick = endcolon + 1;
        if (*starttick != '`')
            break;
        char* endtick = strchr(starttick + 1, '`');
        if (!endtick)
            break;

        // If we're here, we have a role. Record its components.
        dstring rolename = dstring_new_len(startcolon + 1,
                                           endcolon - startcolon - 1);
        dstring rolecontents = dstring_new_len(starttick + 1,
                                               endtick - starttick - 1);

        dstring transformed = PluginManager_apply_role_hooks(
                                    pm,
                                    rolename, rolecontents,
                                    db, post);
        if (transformed) {
            // A plugin was found and transformed the role. Now we'll stitch
            // a new text string together, replacing the role with the
            // transformed text.
            size_t len_before_role = startcolon - dstring_cstr(text);
            dstring newtext = dstring_new_len(dstring_cstr(text),
                                              len_before_role);
            dstring_concat(newtext, transformed);
            dstring_concat_cstr(newtext, endtick + 1);

            dstring_free(text);
            text = newtext;
            
            // p must point into the new string, after the role
            p = dstring_cstr(text) + len_before_role +
                dstring_len(transformed);

            dstring_free(transformed);
        } else {
            // No plugins want this role, so leave it in place.
            p = endtick + 1;
        }

        dstring_free(rolename);
        dstring_free(rolecontents);
    }

    return text;
}


dstring htmlize(PluginManager* pm, DB* db, Post* post) {
    dstring contents = Post_get_contents(post);

    dstring pcontents = insert_paragraphs(contents);
    dstring after_role_hooks = apply_role_hooks(pcontents, pm, db, post);
    dstring after_content_hooks = apply_content_hooks(after_role_hooks, pm,
                                                      db, post);
    dstring_free(pcontents);
    dstring_free(after_role_hooks);

    return after_content_hooks;
}

