#-------------------------------------------------------------------------------
# htmlize: plugin/tt.py
#
# A htmlize plugin that handles the "tt" role.
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
#-------------------------------------------------------------------------------
from htmlize.iplugin import IPlugin


class TtFormatter(IPlugin):
    """ Acts on the 'tt' role, placing the contents inside <tt> tags.
    """
    def get_role_hook(self, role_name):
        return self._tt_hook if role_name == 'tt' else None

    def _tt_hook(self, contents):
        return '<tt>' + contents + '</tt>'

