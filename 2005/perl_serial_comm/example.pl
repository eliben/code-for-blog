#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example of SerialComm usage
#
# Copyright (C) <2006> Eli Bendersky
# License: LGPL (http://www.gnu.org/licenses/lgpl.html)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

use strict;
use warnings;
$|++;

use SerialComm;

# Create a new SerialComm object
#
my $sc = SerialComm->new();

# Initialize it. See the comments above init_comm in SerialComm.pm
# for a list of parameters.
#
my $msg = $sc->init_comm( 
                'portname'  => '\\\\.\\COM13', 
                'baudrate'  => 38400,
                'parity'    => 'none',
                'stopbits'  => 1,
                'csocket_log_file' => 'csocket_log.txt');

die "init_comm error: $msg\n" unless $msg eq "";

# Send data to the serial port
# 
$sc->send_data("Hello !!");

while (1)
{
    # Issue a non-blocking serial port read. If no new data arrived,
    # it returns immediately with an empty string.
    #
    my $msg = $sc->try_receive_data();
    
    if ($msg ne "")
    {
        print "Got new message: $msg\n";
    }
    
    sleep(1);
}

