#-------------------------------------------------------------------------------
# htmlize: plugin hook specs.
#
# These are the hooks plugins may want to implement.
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
#-------------------------------------------------------------------------------
import pluggy

hookspec = pluggy.HookspecMarker("htmlize")

@hookspec(firstresult=True)
def htmlize_role_handler(role_name):
    """Return a function accepting role contents.

    The function will be called with a single argument - the role contents, and
    should return what the role gets replaced with.
    """
    pass

@hookspec
def htmlize_contents():
    """Return a function accepting full document contents.

    The function will be called with a single argument - the document contents
    (after paragraph splitting and role processing), and should return the
    transformed contents.
    """
    pass
