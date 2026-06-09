#-------------------------------------------------------------------------------
# htmlize: plugin/tt.py
#
# A htmlize plugin that handles the "tt" role.
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
#-------------------------------------------------------------------------------
import htmlize


@htmlize.hookimpl
def htmlize_role_handler(role_name):
    def hook(contents):
        return f'<tt>{contents}</tt>'

    if role_name == 'tt':
        return hook
    else:
        return None
