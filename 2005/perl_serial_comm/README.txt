This is an implementation of serial port (RS232) communication 
for Perl on Windows platforms. 

Included is a small utility written in C++ - csocket. This is a
serial port to socket bridge. It waits for data to arrive to
the serial port and forwards it to a socket. It also waits for 
data to arrive to the socket and forwards it to the serial port.

The source code for csocket includes Ramon De Klein's CSerial 
class, in files Serial.h and Serial.cpp, and my socket to serial 
bridge implementation in csocket.cpp
It should all compile without problems with VC++ 6 and later. The
csocket.exe executable was compiled with VC++ 6 in Release mode.

A Perl class to make use of csocket is included - SerialComm.pm,
along with an example of usage - example.pl

In my experience, using SerialComm with csocket is a great way to
communicate on the serial port on Windows machines with Perl. I've
been employing this technique successfully at work for many tasks
involving the serial port. A big advantage of this approach is that
it can run from the default installation of ActiveState Perl, 
without the need to install modules that require a compiler. Just
drag SerialComm.pm and csocket.exe with you, and everything should
work.

Eli Bendersky
eliben@gmail.com

