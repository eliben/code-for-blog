#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SerialComm - Perl class for serial port communication
# via the csocket serial to socket bridge.
#
# Copyright (C) <2006> Eli Bendersky
# License: LGPL (http://www.gnu.org/licenses/lgpl.html)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

package SerialComm;

use strict;
use warnings;
$|++;
use Time::HiRes qw/usleep gettimeofday/;
use Carp;
use Win32::Process;
use IO::Socket;


sub new
{
	my $class = shift;
	my $self = {};
	
	$self->{'socket_to'} = undef;
	$self->{'socket_from'} = undef;
	$self->{'slave_process'} = undef;
	
	bless($self, $class);
	return $self;
}


# Initializes the communication module, given the channel parameters:
#
# path 				- Path to the csocket child application
# socket_num_to     - socket to use in communication with csocket
# socket_num_from   - socket to use in communication with csocket
# csocket_log_file	- log file for csocket to write into
# portname 			- serial port name (pass a full name, e.g. '\\\\.\\COM1')
# baudrate			- communication baudrate (e.g. 38400)
# parity			- 'even', 'odd' or 'none'
# stopbits			- 1 or 2
#
# Returns "" on success, an error message otherwise
#
sub init_comm
{
	my $self = shift;
	my %args = (	'path'				=> 'csocket.exe',
					'socket_num_to'		=> 14441,
					'socket_num_from'	=> 14442,
					'csocket_log_file'	=> "",
					@_);
					
	_validate_args_exist(\%args, 'portname', 'baudrate', 'parity', 'stopbits');

	my $param = join(',', ($args{'socket_num_to'}, $args{'socket_num_from'}, $args{'portname'}, $args{'baudrate'}, $args{'parity'}, $args{'stopbits'}));
	
	# Create two sockets, one for sending data (master -> slave) and one for receiving data (slave -> master) 
	#
	my $listener_socket_to = IO::Socket::INET->new(LocalPort => $args{'socket_num_to'}, Listen => 5, Reuse => 1);
	return "Can't create socket_to: $!" unless $listener_socket_to;
	#~ print "Listening for connections on port $args{'port_num_to'}\n";
	
	my $listener_socket_from = IO::Socket::INET->new(LocalPort => $args{'socket_num_from'}, Listen => 5, Reuse => 1);
	return "Can't create socket_from: $!" unless $listener_socket_from;
	#~ print "Listening for connections on port $args{'port_num_from'}\n";
	
	# Create a new 'csocket' slave process 
	Win32::Process::Create($self->{'slave_process'}, $args{'path'}, "csocket $param $args{'csocket_log_file'}", 0, NORMAL_PRIORITY_CLASS, ".") or return ErrorReport();
	
	# wait until the slave connects on both sockets
	#
	$self->{'socket_to'} = $listener_socket_to->accept();
	#~ print "Got connection on socket_to\n";
	
	$self->{'socket_from'} = $listener_socket_from->accept();
	#~ print "Got connection on socket_from\n";
	#~ print "Sockets connected\n";
	
	# Make the slave -> master socket non-blocking, so that calls to sysread() won't block
	#
	my $nonblocking = 1;
	ioctl($self->{'socket_from'}, 0x8004667E, \$nonblocking);
	
	# Give the slave some time to get ready
	#
	usleep(100_000);
	return "";
}


sub cleanup
{
	my $self = shift;
	
	usleep(100_000);
	$self->{'slave_process'}->Kill(0) if defined $self->{'socket_to'};
	$self->{'socket_to'}->close() if defined $self->{'socket_to'};
	$self->{'socket_from'}->close() if defined $self->{'socket_from'};
	usleep(100_000);
}


sub DESTROY
{
	my $self = shift;
	$self->cleanup();
}


# Tries to read data from the slave. Returns an empty buffer if nothing was received, and a non-empty
# buffer will be returned with the received data, if something was received.
#
# Note: returns immediately in any case, so if you include it in a polling loop, add some sleep into
# it so that it won't clog the CPU
#
sub try_receive_data
{
	my $self = shift;
	my $size = $_[0] || 1000;
	my $buf;
	my $nbytes = sysread($self->{'socket_from'}, $buf, $size);

	if (defined($nbytes) and $nbytes > 0)
	{
		return $buf;
	}
	else
	{
		return "";
	}
}


# Blocks until it receives at least N bytes
#
sub receive_n_bytes
{
	my $self = shift;
	my $n = (@_);
	my $buf = "";
	
	while (length($buf) < $n)
	{
		$buf .= try_receive_data();
	}
	
	return $buf;
}


sub send_data
{
	my $self = shift;
	my ($buf) = @_;
	print {$self->{'socket_to'}} $buf;
}


sub ErrorReport
{
	print Win32::FormatMessage(Win32::GetLastError());
}


sub _validate_args_exist
{
    my ($args, @required) = @_;
    
    foreach my $req_arg (@required)
    {
        croak "Required argument $req_arg not found" unless exists $args->{$req_arg};
    }
}


1;

