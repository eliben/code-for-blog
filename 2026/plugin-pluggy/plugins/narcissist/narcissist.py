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
from htmlize.iplugin import IPlugin


class Narcissist(IPlugin):
    def __init__(self, post, db):
        super().__init__(post, db)
        self.repl = '<b>I ({0})</b>'.format(self.post.author)

    def get_contents_hook(self):
        return self._contents_hook

    def _contents_hook(self, contents):
        return re.sub(r'\bI\b', self.repl, contents)

