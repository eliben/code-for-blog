# Plugins can import the hookimpl marker from this module.
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
import pluggy

hookimpl = pluggy.HookimplMarker("htmlize")
