#-------------------------------------------------------------------------------
# htmlize: htmlize_main.py
#
# Main user-facing program. Usage: pipe some input text to its stdin.
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
#-------------------------------------------------------------------------------
from datetime import datetime
import os, sys
from htmlize.core import htmlize
from htmlize.db import DB, Post
from htmlize.iplugin import discover_plugins


if __name__ == '__main__':
    # Look for plugins in the plugins/ directory which lives in the same
    # place with this program.
    mydir = os.path.dirname(sys.argv[0])
    plugins = discover_plugins([os.path.join(mydir, 'plugins')])

    contents = sys.stdin.read()
    db = DB()
    post = db.create_new_post(
                author='eliben',
                date=datetime.today(),
                title='Hello world',
                contents=contents)

    print(htmlize(post, db, plugins))

