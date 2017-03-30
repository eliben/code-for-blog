# Frame decoding with a state machine.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
class ProtocolWrapper(object):
    def __init__(self,
            header=0x61,
            footer=0x62,
            dle=0xAB,
            after_dle_func=lambda x: x):
        self.header = header
        self.footer = footer
        self.dle = dle
        self.after_dle_func = after_dle_func

        self.state = self.WAIT_HEADER
        self.frame = bytearray()

    # internal state
    (WAIT_HEADER, IN_MSG, AFTER_DLE) = range(3)

    def input(self, byte):
        """ Receive a byte.
            If this byte completes a frame, the
            frame is returned. Otherwise, None
            is returned.
        """
        print('got byte: "{0}"'.format(byte))
        if self.state == self.WAIT_HEADER:
            if byte == self.header:
                self.state = self.IN_MSG
                self.frame = bytearray()
            return None
        elif self.state == self.IN_MSG:
            if byte == self.footer:
                self.state = self.WAIT_HEADER
                return self.frame
            elif byte == self.dle:
                self.state = self.AFTER_DLE
            else:
                self.frame.append(byte)
            return None
        elif self.state == self.AFTER_DLE:
            self.frame.append(self.after_dle_func(byte))
            self.state = self.IN_MSG
            return None
        else:
            raise AssertionError()


if __name__ == "__main__":
    import codecs
    bytes = bytes([0x70, 0x24,
                   0x61, 0x99, 0xAF, 0xD1, 0x62,
                   0x56, 0x62,
                   0x61, 0xAB, 0xAB, 0x14, 0xAB, 0x62, 0x62,
                  ])

    pw = ProtocolWrapper()

    for byte in bytes:
        frame = pw.input(byte)
        if frame:
            print('Got frame:', codecs.encode(frame, 'hex'))
