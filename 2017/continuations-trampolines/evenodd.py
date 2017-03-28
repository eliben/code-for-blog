# Examples of mutual recursion with even/odd.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

def is_even(n):
    if n == 0:
        return True
    else:
        return is_odd(n - 1)

def is_odd(n):
    if n == 0:
        return False
    else:
        return is_even(n - 1)


if __name__ == '__main__':
    print(is_even(800))
