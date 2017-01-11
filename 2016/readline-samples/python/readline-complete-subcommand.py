# Subcommand completion with the readline module.
#
# Tested with Python 3.4
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import glob
import readline

def make_subcommand_completer(commands):
    def custom_complete(text, state):
        # Simplistic parsing of the command-line so far. We want to know if the
        # user is still entering the command, or if the command is already there
        # and now we have to complete the subcommand.
        linebuf = readline.get_line_buffer()
        parts = linebuf.split()

        if len(parts) >= 1 and linebuf.endswith(' '):
            # If we're past the first part and there is whitespace at the end of
            # the buffer, it means we're already completing the next part.
            parts.append('')

        if len(parts) <= 1:
            matches = [w + ' ' for w in commands.keys()
                               if w.startswith(text)] + [None]
            return matches[state]
        elif len(parts) >= 2:
            command = parts[0]

            if command == 'file':
                # Treat 'file' specially, by looking for matching files in the
                # current directory.
                matches = [w + ' ' for w in glob.glob(text + '*')] + [None]
            else:
                matches = [w + ' ' for w in commands[command]
                                   if w.startswith(parts[1])] + [None]
            return matches[state]
    return custom_complete

def main():
    commands = {
        'file': {},
        'eat': {'breakfast', 'dinner', 'lunch', 'snack'},
        'play': {'cards', 'chess', 'go'},
        'walk': {'left', 'right', 'straight'},
    }
    readline.parse_and_bind('tab: complete')
    readline.set_completer(make_subcommand_completer(commands))

    # Use the default readline completer delims; Python's readline adds '-'
    # which makes filename completion funky (for files that contain '-').
    readline.set_completer_delims(" \t\n\"\\'`@$><=;|&{(")

    try:
        while True:
            s = input('>> ').strip()
            print('[{0}]'.format(s))
    except (EOFError, KeyboardInterrupt) as e:
        print('\nShutting down...')

if __name__ == '__main__':
    main()
