#-------------------------------------------------------------------------------
# htmlize: htmlize/core.py
#
# The core functionality of htmlize.
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
#-------------------------------------------------------------------------------
from collections import namedtuple
import re


# Regex for matching/capturing role text.
# E.g. :name:`text` - first capture group is "name", second group is "text"
#
ROLE_REGEX = re.compile(r':(\w+):`([^`]*)`')

RoleMatch = namedtuple('RoleMatch', 'name contents')


def htmlize(post, db, plugins=[]):
    """ pass
    """
    contents = post.contents

    # Plugins are classes - we need to instantiate them to get objects.
    plugins = [P(post, db) for P in plugins]

    # Split the contents to paragraphs
    paragraphs = re.split(r'\n\n+', contents)
    for i, p in enumerate(paragraphs):
        paragraphs[i] = '<p>' + p.replace('\n', ' ') + '</p>'

    contents = '\n\n'.join(paragraphs)

    # Find roles in the contents. Create a list of parts, where each
    # part is either text that has no roles in it, or a RoleMatch
    # object.
    pos = 0
    parts = []
    while True:
        match = ROLE_REGEX.search(contents, pos)
        if match is None:
            parts.append(contents[pos:])
            break
        parts.append(contents[pos:match.start()])
        parts.append(RoleMatch(match.group(1), match.group(2)))
        pos = match.end()

    # Ask plugins to act on roles
    for i, part in enumerate(parts):
        if isinstance(part, RoleMatch):
            parts[i] = _plugin_replace_role(
                            part.name, part.contents, plugins)

    # Build full contents back again, and ask plugins to act on
    # contents.
    contents = ''.join(parts)
    for p in plugins:
        contents_hook = p.get_contents_hook()
        if contents_hook:
            contents = contents_hook(contents)

    return contents


def _plugin_replace_role(name, contents, plugins):
    """ The first plugin that handles this role is used.
    """
    for p in plugins:
        role_hook = p.get_role_hook(name)
        if role_hook:
            return role_hook(contents)
    # If no plugin handling this role is found, return its original form
    return ':{0}:`{1}`'.format(name, contents)


