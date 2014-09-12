#-------------------------------------------------------------------------------
# htmlize: htmlize/db.py
#
# A simplistic DB holding posts with some metadata.
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
#-------------------------------------------------------------------------------

class Post(object):
    def __init__(self, id, author, date, title, contents):
        self.id = id
        self.author = author
        self.date = date
        self.title = title
        self.contents = contents


class DB(object):
    def __init__(self):
        self.posts = {}
        self.id = 0
        
    def create_new_post(self, author, date, title, contents):
        post = self.posts[self.id] = Post(self.id, author, date,
                                          title, contents)
        self.id += 1
        return post

    def get_post(self, id):
        return self.posts[id]

    def get_post_by_title(self, title):
        # In a realistic implementation this would probably be indexes if
        # frequent access by title is required.
        for id, post in self.posts.items():
            if post.title == title:
                return post
        return None


