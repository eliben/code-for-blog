import json
import re
import sys

def update_json_values(obj, fn):
    """Recursively copy `obj`, replacing each scalar value with `fn(value)`."""
    if isinstance(obj, dict):
        return {k: update_json_values(v, fn) for k, v in obj.items()}
    if isinstance(obj, list):
        return [update_json_values(v, fn) for i, v in enumerate(obj)]
    return fn(obj)


if __name__ == '__main__':
    if len(sys.argv) > 1:
        if sys.argv[1] == "supports":
            # support all renderers
            sys.exit(0)

    print('python-narcissist loaded', file=sys.stderr)

    # load both the context and the book representations from stdin
    context, book = json.load(sys.stdin)

    author = context['config']['book']['authors'][0]
    repl = '<b>I ({0})</b>'.format(author)

    def renamer(value):
        if isinstance(value, str):
            return re.sub(r'\bI\b', repl, value)
        else:
            return value
    book = update_json_values(book, renamer)

    # Print updated book contents
    print(json.dumps(book))
