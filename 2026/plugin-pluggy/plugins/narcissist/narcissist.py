#-------------------------------------------------------------------------------
# htmlize: plugin/narcissist.py
#
# A htmlize plugin that places all occurrences of "I" in bold, adding the
# username.
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
#-------------------------------------------------------------------------------
import re
import htmlize


@htmlize.hookimpl
def htmlize_contents(post, db):
    repl = f'<b>I ({post.author})</b>'

    def hook(contents):
        return re.sub(r'\bI\b', repl, contents)

    return hook
