//
// db.h: Mock database interface. It has no real functionality, consisting
// of some stubs that simulate what a real DB interface would look like.
// 
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#ifndef DB_H
#define DB_H

#include "clib/dstring.h"

// Opaque objects: a DB and a Post
typedef struct DB_t DB;
typedef struct Post_t Post;


// Methods on a Post object.
// The Post object takes ownership of all the dynamic strings passed to
// it; its accessors return references to these objects, which must not be
// freed or modified.
//
Post* Post_new(int id, dstring author, dstring contents);
void Post_free(Post* post);
const dstring Post_get_contents(Post* post);
const dstring Post_get_author(Post* post);


#endif /* DB_H */

