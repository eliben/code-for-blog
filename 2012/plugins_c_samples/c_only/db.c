//
// db.c: Mock database implementation
// 
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#include "db.h"

#include "clib/mem.h"


struct DB_t {
    dstring dbname;
};


struct Post_t {
    int id;
    dstring author;
    dstring contents;
};


Post* Post_new(int id, dstring author, dstring contents) {
    Post* post = mem_alloc(sizeof(*post));
    post->id = id;
    post->author = author;
    post->contents = contents;
    return post;
}


void Post_free(Post* post) {
    dstring_free(post->author);
    dstring_free(post->contents);
    mem_free(post);
}


const dstring Post_get_contents(Post* post) {
    return post->contents;
}


const dstring Post_get_author(Post* post) {
    return post->author;
}

