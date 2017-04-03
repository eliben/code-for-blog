# Frame decoding with a state machine.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

# The framing protocol is as described in
# http://eli.thegreenplace.net/2009/08/29/co-routines-as-an-alternative-to-state-machines
# That article also discusses an alternative implementation using co-routines.
HEADER = 0x61
FOOTER = 0x62
DLE = 0xAB


# Basic state machine using explicit state.
class ProtocolWrapper:
    def __init__(self):
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
        if self.state == self.WAIT_HEADER:
            if byte == HEADER:
                self.state = self.IN_MSG
                self.frame = bytearray()
            return None
        elif self.state == self.IN_MSG:
            if byte == FOOTER:
                self.state = self.WAIT_HEADER
                return self.frame
            elif byte == DLE:
                self.state = self.AFTER_DLE
            else:
                self.frame.append(byte)
            return None
        elif self.state == self.AFTER_DLE:
            self.frame.append(byte)
            self.state = self.IN_MSG
            return None
        else:
            raise AssertionError()


# State machine using recursive tail calls. Every state is handled by a
# state_<name> method in this class, and state transitions are modeled by
# calling these methods. Note that this means the call stack may grow
# indefinitely for arbitrary state machines - this should be addressed with
# trampolines.
#
# Avoiding python generators on purpose to demonstrate a point -
# generators/yield provide for a better general approach in Python.
class ProtocolStateRecursive:
    def __init__(self, input_feed, frame_callback):
        """Creates a new protocol.

        input_feed:
            A function that provides the next byte when called. If it returns
            None, the protocol stops.
        frame_callback:
            A function that will be called for every decoded frame, with the
            frame as the single argument.
        """
        self.input_feed = input_feed
        self.frame_callback = frame_callback
        self.frame = bytearray()
        self.state_wait_header()

    def state_wait_header(self):
        byte = self.input_feed()
        if byte is None:
            return
        elif byte == HEADER:
            self.frame = bytearray()
            self.state_in_msg()
        else:
            self.state_wait_header()

    def state_in_msg(self):
        byte = self.input_feed()
        if byte is None:
            return
        elif byte == FOOTER:
            self.frame_callback(self.frame)
            self.state_wait_header()
        elif byte == DLE:
            self.state_after_dle()
        else:
            self.frame.append(byte)
            self.state_in_msg()

    def state_after_dle(self):
        byte = self.input_feed()
        if byte is None:
            return
        else:
            self.frame.append(byte)
            self.state_in_msg()


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

    byteptr = 0
    def bytes_feeder():
        global byteptr
        if byteptr >= len(bytes):
            return None
        else:
            nextbyte = bytes[byteptr]
            byteptr += 1
            return nextbyte

    def frame_callback(frame):
        print('Callback got frame:', codecs.encode(frame, 'hex'))

    psr = ProtocolStateRecursive(bytes_feeder, frame_callback)
