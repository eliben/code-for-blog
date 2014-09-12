//
// htmlize_main.c: The main program
//
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#include "clib/die.h"
#include "clib/mem.h"
#include "db.h"
#include "htmlize.h"
#include "plugin_discovery.h"
#include "plugin_manager.h"
#include <stdio.h>


// Read a whole file into a dstring and return it
dstring read_whole_file(FILE* file) {
    static const size_t BUF_SIZE = 1024;
    char buf[BUF_SIZE];
    dstring s = dstring_empty();

    while (fgets(buf, BUF_SIZE, file)) {
        dstring_concat_cstr(s, buf);
    }

    return s;
}


int main(int argc, const char* argv[]) {
    dstring contents = read_whole_file(stdin);

    printf("Original file:\n--------\n");
    printf("%s", dstring_cstr(contents));
    printf("\n--------\n\n");
    
    // The Post object takes ownership of the strings passed to it
    Post* post = Post_new(42, dstring_new("Someone"), contents);

    // Perform plugin discovery in the "plugins" directory relative to the
    // working directory.
    PluginManager* pm = PluginManager_new();
    dstring dirname = dstring_new("plugins");
    void* pdstate = discover_plugins(dirname, pm);  
    dstring_free(dirname);

    dstring htmlized = htmlize(pm, 0, post);

    printf("\nHtmlized:\n--------\n");
    printf("%s", dstring_cstr(htmlized));
    printf("\n--------\n");

    Post_free(post);
    dstring_free(htmlized);
    PluginManager_free(pm);
    cleanup_plugins(pdstate);

    return 0;
}

