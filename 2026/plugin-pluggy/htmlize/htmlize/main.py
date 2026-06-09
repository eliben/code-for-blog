from datetime import datetime
import os, sys

import pluggy

from htmlize import hookspecs
from htmlize.core import htmlize
from htmlize.db import DB, Post


def main():
    pm = pluggy.PluginManager("htmlize")
    pm.add_hookspecs(hookspecs)
    pm.load_setuptools_entrypoints("htmlize")

    contents = sys.stdin.read()
    db = DB()
    post = db.create_new_post(
                author='eliben',
                date=datetime.today(),
                title='Hello world',
                contents=contents)

    print(htmlize(post, db, pm))


if __name__ == "__main__":
    main()
