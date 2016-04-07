# Protocol for the serial port sample.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import unittest, re, sys, random
sys.path.insert(0, '..')
from protocolwrapper import *


def a2s(arr):
    """ Array of integer byte values --> binary string
    """
    return ''.join(chr(b) for b in arr)


class TestProtocolWrapper(unittest.TestCase):
    def assertLastError(self, pw, error_like):
        self.failUnless(re.search(error_like, pw.last_error),
            "\nExpected error matching: %s\nGot: %s" %
                (error_like, pw.last_error))

    def input_seq(self, pw, seq):
        return [pw.input(chr(s)) for s in seq]

    def test_no_header_from_beginning(self):
        pw = ProtocolWrapper()
        rc = pw.input('\x45')
        self.assertEqual(rc, ProtocolStatus.ERROR)
        self.assertLastError(pw, 'Expected header')

        pw.input('\x82')
        self.assertEqual(rc, ProtocolStatus.ERROR)
        self.assertLastError(pw, 'Expected header')

    def test_minimal_message(self):
        pw = ProtocolWrapper()

        self.assertEqual(self.input_seq(pw, [0x81, 0x83]),
            [ProtocolStatus.START_MSG, ProtocolStatus.MSG_OK])
        self.assertEqual(pw.last_message, '')

        pw = ProtocolWrapper()
        self.assertEqual(self.input_seq(pw, [0x81, 0x11, 0x83]),
            [ProtocolStatus.START_MSG, ProtocolStatus.IN_MSG, ProtocolStatus.MSG_OK])
        self.assertEqual(pw.last_message, '\x11')

    def test_error_recovery(self):
        pw = ProtocolWrapper()
        self.assertEqual(self.input_seq(pw, [0x45, 0x81, 0x83]),
            [   ProtocolStatus.ERROR, ProtocolStatus.START_MSG,
                ProtocolStatus.MSG_OK])
        self.assertEqual(pw.last_message, '')

    def test_large_message(self):
        pw = ProtocolWrapper(keep_header=True, keep_footer=True)
        msg = [0x81, 0x13] + [0x55] * 5000 + [0x83]
        self.input_seq(pw, msg)
        self.assertEqual(pw.last_message, a2s(msg))

    def test_dle(self):
        pw = ProtocolWrapper()
        self.assertEqual(self.input_seq(pw, [0x81, 0x90, 0x05, 0x83]),
            [   ProtocolStatus.START_MSG, ProtocolStatus.IN_MSG,
                ProtocolStatus.IN_MSG, ProtocolStatus.MSG_OK])
        self.assertEqual(pw.last_message, a2s([0x05]))

        pw = ProtocolWrapper()
        self.input_seq(pw, [0x81, 0x12, 0x90, 0x83, 0x83])
        self.assertEqual(pw.last_message, a2s([0x12, 0x83]))

    def test_chains(self):
        # 3 messages in succession
        pw = ProtocolWrapper()
        rc = self.input_seq(pw, [0x81, 0x15, 0x83])
        self.assertEqual(rc[-1], ProtocolStatus.MSG_OK)
        self.assertEqual(pw.last_message, a2s([0x15]))

        rc = self.input_seq(pw, [0x81, 0x88, 0x20, 0x83])
        self.assertEqual(rc[-1], ProtocolStatus.MSG_OK)
        self.assertEqual(pw.last_message, a2s([0x88, 0x20]))

        rc = self.input_seq(pw, [0x81, 0x01, 0x83])
        self.assertEqual(rc[-1], ProtocolStatus.MSG_OK)
        self.assertEqual(pw.last_message, a2s([0x01]))

        # now with some errors between them
        rc = self.input_seq(pw, [0x01, 0x02, 0x81, 0x12, 0x83])
        self.assertEqual(rc[0], ProtocolStatus.ERROR)
        self.assertEqual(rc[-1], ProtocolStatus.MSG_OK)
        self.assertEqual(pw.last_message, a2s([0x12]))

    def test_keep_header_footer(self):
        seq = [0x81, 0x92, 0x83]

        # keep nothing
        pw = ProtocolWrapper()
        self.input_seq(pw, seq)
        self.assertEqual(pw.last_message, '\x92')

        # keep only header
        pw = ProtocolWrapper(keep_header=True)
        self.input_seq(pw, seq)
        self.assertEqual(pw.last_message, '\x81\x92')

        # keep only footer
        pw = ProtocolWrapper(keep_footer=True)
        self.input_seq(pw, seq)
        self.assertEqual(pw.last_message, '\x92\x83')

        # keep both
        pw = ProtocolWrapper(keep_header=True, keep_footer=True)
        self.input_seq(pw, seq)
        self.assertEqual(pw.last_message, '\x81\x92\x83')

    def test_wrap(self):
        pw = ProtocolWrapper()
        self.assertEqual(pw.wrap(a2s([0x1, 0x2, 0x3])),
            a2s([0x81, 0x1, 0x2, 0x3, 0x83]))
        self.assertEqual(pw.wrap(a2s([0x81, 0x90, 0x3])),
            a2s([0x81, 0x90, 0x81, 0x90, 0x90, 0x3, 0x83]))

        pw = ProtocolWrapper(header='\x10', footer='\x10',
                after_dle_func=lambda x: chr(ord(x) ^ 1))
        self.assertEqual(pw.wrap(a2s([0x1, 0x2, 0x3])),
            a2s([0x10, 0x1, 0x2, 0x3, 0x10]))
        self.assertEqual(pw.wrap(a2s([0x10, 0x2, 0x90])),
            a2s([0x10, 0x90, 0x11, 0x2, 0x90, 0x91, 0x10]))

    def test_roundtrip(self):
        pw = ProtocolWrapper(after_dle_func=lambda x: chr(ord(x) ^ 1))
        for i in xrange(100):
            msglen = random.randint(1, 200)
            msg = a2s(random.randint(0, 255) for l in xrange(msglen))
            wrapped = pw.wrap(msg)
            for b in wrapped: pw.input(b)
            self.assertEqual(pw.last_message, msg)


if __name__ == '__main__':
    unittest.main()
