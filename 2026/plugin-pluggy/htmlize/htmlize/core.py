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

from htmlize import hookspecs

import pluggy


# Regex for matching/capturing role text.
# E.g. :name:`text` - first capture group is "name", second group is "text"
#
ROLE_REGEX = re.compile(r':(\w+):`([^`]*)`')

RoleMatch = namedtuple('RoleMatch', 'name contents')


def htmlize(post, db, plugin_manager):
    contents = post.contents

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
            parts[i] = _plugin_replace_role(part.name, part.contents, plugin_manager)

    # Build full contents back again, and ask plugins to act on
    # contents.
    contents = ''.join(parts)
    for handler in plugin_manager.hook.htmlize_contents():
        contents = handler(contents)
    return contents


def _plugin_replace_role(name, contents, plugin_manager):
    role_handler = plugin_manager.hook.htmlize_role_handler(role_name=name)
    if role_handler is not None:
        return role_handler(contents)
    # If no plugin handling this role is found, return its original form
    return ':{0}:`{1}`'.format(name, contents)
