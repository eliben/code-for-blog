# Sample preprocessor plugin, used for debugging.

import json
import sys


if __name__ == "__main__":
    if len(sys.argv) > 1:  # we check if we received any argument
        if sys.argv[1] == "supports":
            # then we are good to return an exit status code of 0, since the
            # other argument will just be the renderer's name
            sys.exit(0)

    # load both the context and the book representations from stdin
    context, book = json.load(sys.stdin)

    # use stderr for debugging printouts from the preprocessor -- these don't
    # get into the final output
    print("context:", file=sys.stderr)
    print(json.dumps(context, indent=4), file=sys.stderr)
    print("book:", file=sys.stderr)
    print(json.dumps(book, indent=4), file=sys.stderr)

    # Can change contents of chapter like this, before
    # emitting back.
    # Note: 'sections' changed to 'items' was changed in the very latest
    # version of mdbooks.
    # book['items'][0]['Chapter']['content'] = '# Hello'

    # The preprocessor has to print the book back to stdout (whether it was
    # modified or not).
    print(json.dumps(book))
