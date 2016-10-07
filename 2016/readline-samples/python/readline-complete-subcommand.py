# Subcommand completion with the readline module.
#
# Tested with Python 3.4
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import readline


def make_subcommand_completer(commands):
    def custom_complete(text, state):
        linebuf = readline.get_line_buffer()
        parts = linebuf.split()

        # If we're done with the first word, means we're looking for the 2nd
        # part.
        if len(linebuf) == 0 or linebuf.endswith(' '):
            parts.append('')

        if len(parts) == 1:
            return [w for w in commands.keys() if w.startswith(text)][state]
        elif len(parts) == 2:
            return [w for w in commands[parts[0]] if w.startswith(text)][state]
    return custom_complete


def main():
    commands = {
        'season': {'winter', 'spring', 'summer', 'fall'},
        'animal': {'cat', 'dog', 'canary', 'cow', 'hamster'}
    }
    readline.parse_and_bind('tab: complete')
    readline.set_completer(make_subcommand_completer(commands))

    try:
        while True:
            s = input('>> ').strip()
            print('[{0}]'.format(s))
    except (EOFError, KeyboardInterrupt) as e:
        print('\nShutting down...')


if __name__ == '__main__':
    main()
