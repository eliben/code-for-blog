# Simple completion with the readline module.
#
# Tested with Python 3.4
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import readline

def make_completer(vocabulary):
    def custom_complete(text, state):
        # None is returned for the end of the completion session.
        # A space is added to the completion since the Python readline doesn't
        # do this on its own. When a word is fully completed we want to mimic
        # the default readline library behavior of adding a space after it.
        results = [x + ' ' for x in vocabulary if x.startswith(text)] + [None]
        return results[state]
    return custom_complete

def main():
    vocabulary = {'cat', 'dog', 'canary', 'cow', 'hamster'}
    readline.parse_and_bind('tab: complete')
    readline.set_completer(make_completer(vocabulary))

    try:
        while True:
            s = input('>> ').strip()
            print('[{0}]'.format(s))
    except (EOFError, KeyboardInterrupt) as e:
        print('\nShutting down...')

if __name__ == '__main__':
    main()
