# Helper for testing.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import sys
import time


def main():
    count = 1
    while True:
        sys.stdout.write(f'{count} ')
        if count % 20 == 0:
            sys.stdout.write('\n')
        time.sleep(0.05)
        count += 1


if __name__ == '__main__':
    main()
