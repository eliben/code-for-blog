# Protocol for the serial port sample.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
from construct import *


message_crc = Struct('message_crc', ULInt32('crc'))


message_format = Struct('message_format',
    ULInt16('msg_id'),
    ULInt16('dest_addr'),
    Enum(Byte('command_type'),
        RESTART = 0x40,
        RESTART_ACK = 0x80,
        SIGNAL = 0x22,
        _default_ = Pass
    ),
    BitStruct('flags',
        Flag('on'),
        BitField('status', 3),
        Flag('cache'),
        Padding(3)
    ),
    Byte('datalen'),
    Array(lambda ctx: ctx['datalen'], Byte('data')),
    Embed(message_crc)
)


if __name__ == "__main__":
    raw = message_format.build(Container(
        msg_id=0x1234,
        dest_addr=0xacba,
        command_type='RESTART',
        flags=Container(on=1, cache=0, status=4),
        datalen=4,
        data=[0x1, 0xff, 0xff, 0xdd],
        crc=0x12345678))

    print raw.encode('hex')
