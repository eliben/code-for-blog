# Imperative sequence processing in Python.
# Tested with Python 3.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

def process(s):
    result = 0
    for i in s:
        if i % 2 == 0:
            result += i + 1
    return result


if __name__ == '__main__':
    print(process(range(10)))
