##
#
# PerlMIX - the MIX assembler/simulator in Perl
#
#   by Eli Benderksy
#
##
#
# Package MIX::Sim
#
# The MIX virtual machine / simulator. Receives a map
# of memory + MIX devices, and an initial address to 
# execute from. Runs a simulation - the output is an 
# updated memory, registers and devices.
#
# Exported functions:
#
# init_sim 			- resets the simulator
# step_sim			- runs a single instruction
# run_sim			- runs complete simulation (until a HLT 
#					  instruction is encountered)
# interactive_sim	- runs an interactive/debugging simulation,
#					  giving the user full control.
# simulation_ended	- indicator of simulation end (HLT)
# get_mem_ref		- returns a reference to the memory map
# memory_dump		- prints out a memory map dump
# state_dump		- prints out the internal state (registers)
# 
# Usage: see MIX::Asm 
# 
# Interface: the interface of MIX::Sim is not object-oriented, and 
# is not state-less, there will be problems running it in several
# threads (not that you have any reason to do it...)
#
# Note: Assumes auto-generated input (for example from MIX::Asm), hence
# quite sensitive to errors. If you're not sure what's happening in your
# simulation, you can always fire-up interactive_sim - it provides good
# means for debugging (breakpoints, stepping, etc.)
#
package MIX::Sim;

use warnings;
use strict;
use Carp;
use Data::Dumper;
use MIX::Common;
require Exporter;

use vars qw(@ISA @EXPORT $VERSION @EXPORT_OK);

$VERSION 	= 1.0;
@ISA     	= qw(Exporter);
@EXPORT  	= qw(interactive_sim get_mem_ref init_sim simulation_ended run_sim step_sim memory_dump state_dump);

# memory array, contains array refs - words
#
my @mem;

# registers
my $rA;
my $rX;
my @rI;
my $rJ;
my $lc;			# location counter

# time (in 'u' units)
#
my $time;

# flags
my $f_overflow;			# can be 0 - off, 1 - on
my $f_comparison;		# can be 0 - "equal", < 0 - "less" or > 0 - "greater"

# An array of IO devices. Initialized in init_sim
# Each array element is:
# 	{
#		filename	=> filename for the device
#		io_type		=> "ci" = char in, "bio" = bin in/out, "co" = char out, etc
#		handle		=> file handle to the device - will be initialized later
#		block_size	=> block size of the device
#		data		=> for binary devices, the data
#	}
#
# Character devices are treated in a line-oriented way. Each group
# of blocks is read/written as a line, spaces filling the missing
# characters.
#
# Binary devices contain ASCII represantations of MIX words, one
# word per line - like a memory map, just without the address.
#
my @io_device;


my $simulation_ended = 0;


# maps opcodes to handlers
#
my %opcode_map = 
(
	1	=> \&op_add_sub,
	2	=> \&op_add_sub,
	3	=> \&op_mul,
	4 	=> \&op_div,
	5	=> \&op_conv,
	6	=> \&op_shift,
	7	=> \&op_move,
	8 	=> \&op_load,
	9	=> \&op_load,
	10	=> \&op_load,
	11	=> \&op_load,
	12	=> \&op_load,
	13	=> \&op_load,
	14	=> \&op_load,
	15	=> \&op_load,
	16	=> \&op_load,
	17	=> \&op_load,
	18	=> \&op_load,
	19	=> \&op_load,
	20	=> \&op_load,
	21	=> \&op_load,
	22	=> \&op_load,
	23	=> \&op_load,
	24 	=> \&op_store,
	25 	=> \&op_store,
	26 	=> \&op_store,
	27 	=> \&op_store,
	28 	=> \&op_store,
	29 	=> \&op_store,
	30 	=> \&op_store,
	31 	=> \&op_store,
	32 	=> \&op_store,
	33 	=> \&op_store,
	34 	=> \&op_jump,
	35 	=> \&op_ioc,
	36 	=> \&op_input,
	37	=> \&op_output,
	38 	=> \&op_jump,
	39	=> \&op_jump,
	40	=> \&op_jump,
	41	=> \&op_jump,
	42	=> \&op_jump,
	43	=> \&op_jump,
	44	=> \&op_jump,
	45	=> \&op_jump,
	46	=> \&op_jump,
	47	=> \&op_jump,
	48	=> \&op_addr_transfer,
	49	=> \&op_addr_transfer,
	50	=> \&op_addr_transfer,
	51	=> \&op_addr_transfer,
	52	=> \&op_addr_transfer,
	53	=> \&op_addr_transfer,
	54	=> \&op_addr_transfer,
	55	=> \&op_addr_transfer,
	56	=> \&op_cmp,
	57	=> \&op_cmp,
	58	=> \&op_cmp,
	59	=> \&op_cmp,
	60	=> \&op_cmp,
	61	=> \&op_cmp,
	62	=> \&op_cmp,
	63	=> \&op_cmp
);


my $saved_mem_file;
my $saved_mem_ref;
my $saved_init_addr;



#------------------------------------------------------------------------------#


# Initializes the simulator.
#
# Arguments:
#
# -init_addr		- the address to start execution from
# -mem_file			- a memory-map file (has precedence over -mem_ref)
# -mem_ref			- a reference to a memory-map
# -device_dir		- directory with the device files
#
sub init_sim
{
	my $args = 
	{
		-init_addr	=> 0,
		-mem_file	=> undef,
		-mem_ref	=> undef,
		-device_dir	=> './',
		@_,
	};
	
	$args->{-device_dir} .= '/' unless $args->{-device_dir} =~ /\/^/;
	
	# init memory
	for (my $i = 0; $i < get_mix_mem_size(); ++$i)
	{
		$mem[$i] = empty_word();
	}
	
	$rA = empty_word();
	$rX = empty_word();
	$rJ = empty_word();
	$rI[$_] = empty_word()
		foreach (1 .. 6);

	$f_overflow = 0;
	$f_comparison = 0;
	$time = 0;
	$lc = $args->{-init_addr};
	$simulation_ended = 0;
	@io_device = ();

	# init IO devices
	#
	foreach my $n (0 .. 15)
	{
		if ($n >= 0 and $n <= 7)
		{
			push(@io_device, {filename => "tape${n}.dev", io_type => "bio", block_size => 100, data => undef});
		}
		elsif ($n >= 8 and $n <= 15)
		{
			my $m = $n - 8;
			push(@io_device, {filename => "disk${m}.dev", io_type => "bio", block_size => 100, data => undef});
		}
	}
	
	push(@io_device, {filename => "cardrd.dev", io_type => "ci", block_size => 16});
	push(@io_device, {filename => "cardwr.dev", io_type => "co", block_size => 16});
	push(@io_device, {filename => "printer.dev", io_type => "co", block_size => 24});
	push(@io_device, {filename => "stdio", io_type => "cio", block_size => 14});
	push(@io_device, {filename => "paper.dev", io_type => "ci", block_size => 14});
	
	foreach my $dev (@io_device)
	{
		$dev->{filename} = $args->{-device_dir} . $dev->{filename};
	}
	
	$saved_mem_file = $args->{-mem_file};
	$saved_mem_ref = $args->{-mem_ref};
	$saved_init_addr = $args->{-init_addr};
	
	if (defined $args->{-mem_file})
	{
		load_memory_from_text_file($args->{-mem_file});
	}
	elsif (defined $args->{-mem_ref})
	{
		@mem = @{$args->{-mem_ref}};
	}
	else
	{
		warn("No memory file or reference given to the simulator\n");
	}
}


sub simulation_ended
{
	return $simulation_ended;
}


sub fetch_next_instruction
{
	return @{$mem[$lc]};
}


# Executes one instruction
#
sub step_sim
{
	address_is_legal($lc)
		or runtime_error("location counter out of memory bounds");
	
	my @word = fetch_next_instruction();
	
	my $opcode = $word[5];
	my $F = $word[4];
	
	if ($opcode == 5 and $F == 2)		# HLT
	{
		$simulation_ended = 1;
		return;
	}
	elsif ($opcode == 0)	# NOP
	{
		$lc++;
		return;
	}
	else
	{
		# Dispatch the instruction to the appropriate handler,
		# based on the opcode.
		#
		if (defined $opcode_map{$opcode})
		{
			my $op_func = $opcode_map{$opcode};
			$op_func->(@word);			
			$lc++;
		}
		else
		{
			runtime_error("illegal opcode: $opcode");
		}
	}
}


sub get_mem_ref
{
	return \@mem;
}


# Simulates the MIX code until a HLT instruction is
# incountered.
#
sub run_sim
{
	# step through the whole program
	#
	until (simulation_ended())
	{
		step_sim();
	}
	
	# update the binary devices
	#
	foreach my $devref (@io_device)
	{
		next unless is_binary_device($devref) and defined $devref->{data};
		
		my $fh = $devref->{handle};
		close $fh if defined $fh;
		
		unless (open($fh, ">$devref->{filename}"))
		{	
			warn "Unable to write device $devref->{filename}\n";
			next;
		}
		
		foreach my $block_n (keys %{$devref->{data}})
		{
			print $fh "$block_n\n";
			
			for (my $i = 0; $i < $devref->{block_size}; ++$i)
			{
				print $fh sprintf("%2s %2s %2s %2s %2s %2s\n", @{$devref->{data}->{$block_n}->[$i]});
			}
		}
		
		close $fh;
	}
}


sub interactive_error
{
	my ($msg) = @_;
	
	print "$msg. Type 'h' for help\n";
}


sub interactive_sim
{
	local $| = 1;
	my %breakpoints;
	
	print "\nWelcome to MIXSim interaction !\n\n";
	
	interaction: while (1)
	{
		printf "[%4s]> ", $lc;
		my $command = <>;
		chomp($command);
		
		# strip leading and trailing whitespace
		$command =~ s/^\s+//;
		$command =~ s/\s+$//;
		
		my @toks = split('\s+', $command);
		next if @toks == 0;
		
		if ($command eq "s")
		{
			step_sim();
			
			print "Simulation ended (HLT)\n" if (simulation_ended());
			
		}
		elsif ($command eq "c" or $command eq "cl")
		{
			step_loop: while (1)
			{
				if (exists $breakpoints{$lc})
				{
					print "Breakpoint stop at address $lc\n";
					last step_loop;
				}
				
				if (simulation_ended())
				{
					print "Simulation ended (HLT)\n" if (simulation_ended());
					last step_loop;
				}
				
				print "$lc\n" if $command eq "cl";
				step_sim();
			}
		}
		elsif ($command eq "rst")
		{
			if (defined $saved_mem_file)
			{
				init_sim(-mem_file => $saved_mem_file, -init_addr => $saved_init_addr);
			}
			elsif (defined $saved_mem_ref)
			{
				init_sim(-mem_ref => $saved_mem_ref, -init_addr => $saved_init_addr);
			}
		}
		elsif ($command eq "r")
		{
			print state_dump(), "\n";
		}
		elsif ($command eq "sr")
		{
			step_sim();
			print state_dump(), "\n";
		}
		elsif ($toks[0] eq "m")
		{
			if (@toks == 1)
			{
				print memory_dump(\@mem);
			}
			elsif (@toks == 2)
			{
				my $addr = $toks[1];
				address_is_legal($addr) or interactive_error("Illegal address $addr");
				printf("%4s : %2s %2s %2s %2s %2s %2s\n", $addr, @{$mem[$addr]});
			}
			else
			{
				interactive_error("Illegal m command");
			}
		}
		elsif ($toks[0] eq "b")
		{
			if (@toks != 2) 
			{
				interactive_error("Illegal b command");
				next;
			}
			
			my $addr = $toks[1];
			
			if (not address_is_legal($addr))  
			{
				interactive_error("Illegal address $addr");
				next;
			}
			
			if (exists $breakpoints{$addr})
			{
				delete($breakpoints{$addr});
				print "Removed breakpoint at $addr\n";
			}
			else
			{
				$breakpoints{$addr} = 1;
				print "Set breakpoint at $addr\n";
			}
		}
		elsif ($command eq "bl")
		{
			my @bkpt_keys = keys %breakpoints;
			
			if (@bkpt_keys == 0)
			{
				print "No breakpoints set\n";
			}
			else
			{
				print "Breakpoints set at:\n";
				
				if (@bkpt_keys == 1)
				{
					print "$bkpt_keys[0]  ";
				}
				else
				{
					foreach my $addr (sort {$a <=> $b} @bkpt_keys)
					{
						print "$addr  ";
					}
				}
				
				print "\n";
			}
		}
		elsif ($command eq "br")
		{
			%breakpoints = ();
		}
		elsif ($command eq "h")
		{
			print "\n*** MIXSim interaction help ***\n\n";
			print "s       \t\t step\n";
			print "c       \t\t continue until next breakpoint or HLT\n";
			print "cl      \t\t same as 'c', with an execution trace\n"; 
			print "rst     \t\t restart simulation (breakpoints remain)\n";
			print "r       \t\t print contents of registers\n";
			print "sr      \t\t step and print contents of registers\n";
			print "m       \t\t print all non-zero memory words\n";
			print "m <addr>\t\t print a memory word at <addr>\n";
			print "b <addr>\t\t set/unset a breakpoint at <addr>\n";
			print "bl      \t\t list all breakpoints\n";
			print "br      \t\t remove all breakpoints\n";
			print "h       \t\t show this help\n";
			print "x or q  \t\t exit interaction\n\n";
		}
		elsif ($command eq "x" or $command eq "q")
		{
			last interaction;
		}
		else
		{
			print "Illegal command. Type 'h' for help\n";
		}
	}
	
	print "\nBye !\n\n";
}


# Returns a state dump - contents of all the registers
#
sub state_dump
{
	my $dump_str = "";

	$dump_str .= sprintf("rA   : %2s %2s %2s %2s %2s %2s\n", @{$rA});
	$dump_str .= sprintf("rX   : %2s %2s %2s %2s %2s %2s\n", @{$rX});

	$dump_str .= sprintf("rI$_  : %2s %2s %2s %2s %2s %2s\n", @{$rI[$_]})
		foreach (1 .. 6);
	
	$dump_str .= "\n";
	$dump_str .= sprintf("rJ   : %2s %2s %2s %2s %2s %2s\n", @{$rJ});
	$dump_str .= sprintf("lc   : %5s\n", $lc);
	$dump_str .= sprintf("ovf  : %2s\n", $f_overflow);
	$dump_str .= sprintf("comp : %2s\n", $f_comparison);
}


# Reports runtime errors - errors that occured during simulation
# as a result of incorrect machine code. $lc is reported
#
sub runtime_error
{
	my ($msg) = @_;
	
	die("Simulation error at address $lc: $msg\n");
}


# Loads all words listed in a text file to the memory
# All other words are assumed to be empty
#
sub load_memory_from_text_file
{
	my $file_name = $_[0];
	
	open(FH, $file_name) 
		or croak("Unable to open memory file $file_name\n");
	
	while (my $line = <FH>)
	{
		chomp($line);		
		next if $line =~ /^\s*$/;			# skip empty lines
		next if $line =~ /^\*/;				# skip full-line comments
		
		$line =~ s/\*.*$//;					# remove end-of-line comments
		
		my @tokens = split(/\s+/, $line);
		next if @tokens == 0;
		
		(@tokens == 7) 
			or die("In file $file_name illegal line #$.\n");
			
		my ($address, @bytes) = @tokens;
		
		validate_address($address);
		validate_sign($bytes[0]);
		validate_byte($_) foreach (@bytes[1..$#bytes]);

		# here the line is valid, so we can load it to memory
		$mem[$address] = \@bytes;
	}
}


# Takes a word, returns an effective address (fully computed
# with index register contents taken into account)
#
sub get_effective_address
{
	my @word = @_;
	
	# base address
	my @A_bytes = @word[0..2];
	my $AA = word2value(\@A_bytes);
	
	# calculate index register's contents
	my $i_reg_contents = 0;
	my $index = $word[3];

	($index >= 0 and $index <= 6)
		or runtime_error("illegal rI register number: $index");

	# $index == 0   => no indexing
	unless ($index == 0)
	{
		my @i_reg_bytes = @{$rI[$index]}[4..5];
		my $i_reg_sign = ${$rI[$index]}[0];
		$i_reg_contents = word2value([$i_reg_sign, @i_reg_bytes]);
	}
	
	return $AA + $i_reg_contents;
}


# given a word, returns effective address, L and R
#
sub full_word_decode
{
	my @word = @_;
	my $M = get_effective_address(@word);
	my ($L, $R) = @{f2range($word[4])};
	
	return [$M, $L, $R];
}


sub flip_sign
{
	my $sign = $_[0];
	
	return $sign eq "+" ? "-" : "+";
}


# Accepts a word W and a range F.
# Builds an "effective" word V, such that the bytes in range F 
# from W enter the rightmost bytes of V
#
sub word2v
{
	my @mem_word = @{$_[0]};
	my ($L, $R) = @_[1..2];
	
	my @V = @{empty_word()};
	
	# if $L is 0, a sign must be copied. 
	# $L is then increased, as the sign was handled
	if ($L == 0)
	{
		$V[0] = $mem_word[0];
		++$L;
	}
	
	my $len = $R - $L;

	# copy (L:R) of mem_word into new_word
	@V[5 - $len .. 5] = @mem_word[$L .. $R];
	
	return \@V;
}


# handler for all load instructions:
# LDA, LDX, LDi, LDAN, LDXN, LDiN 
#
sub op_load
{
	my @op_word = @_;
	my $op = $op_word[5];
	my ($M, $L, $R) = @{full_word_decode(@op_word)};
	
	my @mem_word = @{$mem[$M]};
	my @V = @{word2v(\@mem_word, $L, $R)};
	
	# the same word, just negative, for the loads finishing
	# with N
	my @V_neg = @V;
	$V_neg[0] = flip_sign($V_neg[0]);

	# LDA
	if    ($op == 8)
	{
		$rA = \@V;
	}
	# LDX
	elsif ($op == 15)
	{
		$rX = \@V;
	}
	# LDi
	elsif ($op > 8 and $op < 15)
	{
		$rI[$op - 8] = \@V;
	}
	# LDAN
	elsif ($op == 16)
	{
		$rA = \@V_neg;
	}
	# LDXN
	elsif ($op == 23)
	{
		$rX = \@V_neg;
	}
	# LDiN
	elsif ($op > 16 and $op < 23)
	{
		$rI[$op - 16] = \@V_neg;
	}
	else
	{
		internal_error("illegal op_load op $op");
	}
	
	$time += 2;
}


# handler for all store instructions:
# STA, STX, STi, STJ, STZ 
#
sub op_store
{
	my @op_word = @_;
	my $op = $op_word[5];
	my ($M, $L, $R) = @{full_word_decode(@op_word)};
	
	my @reg_word;
	
	# STA
	if ($op == 24)
	{
		@reg_word = @{$rA};
	}
	# STX
	elsif ($op == 31)
	{
		@reg_word = @{$rX};
	}
	# STi
	elsif ($op > 24 and $op < 31)
	{
		@reg_word = @{$rI[$op - 24]};
	}
	# STJ
	elsif ($op == 32)
	{
		@reg_word = @{$rJ};
	}
	# STZ
	elsif ($op == 33)
	{
		@reg_word = @{empty_word()};
	}
	else
	{
		internal_error("illegal op_store op $op");
	}
	
	my @new_word = @{$mem[$M]};
	
	if ($L == 0)
	{
		$new_word[0] = $reg_word[0];
		++$L;
	}
	
	my $len = $R - $L;
	
	@new_word[$L .. $R] = @reg_word[5 - $len .. 5];
	
	$mem[$M] = \@new_word;
	
	$time += 2;
}


# handler for ADD and SUB instructions
#
sub op_add_sub
{
	my @op_word = @_;
	my $op = $op_word[5];
	my ($M, $L, $R) = @{full_word_decode(@op_word)};
	
	my @V = @{word2v($mem[$M], $L, $R)};
	my @reg_word = @{$rA};
	
	my $V_value = word2value(\@V);
	my $reg_value = word2value(\@reg_word);
	my $new_value;
	
	# ADD
	if ($op == 1)
	{
		$new_value = $reg_value + $V_value;
	}
	# SUB
	elsif ($op == 2)
	{
		$new_value = $reg_value - $V_value;
	}
	else
	{
		internal_error("illegal op_add_sub op $op");
	}
	
	$f_overflow = 1 
		if ($new_value > get_mix_max_word_value() or $new_value < -get_mix_max_word_value());
	
	$rA = value2word($new_value);
	
	$time += 2;
}


# handler for the MUL instruction
#
sub op_mul
{
	my @op_word = @_;
	my $op = $op_word[5];
	my ($M, $L, $R) = @{full_word_decode(@op_word)};
	
	my @V = @{word2v($mem[$M], $L, $R)};
	
	my @reg_word = @{$rA};
	
	my $V_value = word2value(\@V);
	my $reg_value = word2value(\@reg_word);
	
	my $new_value = $V_value * $reg_value;

	# the multiplication result enters rAX (low to X and 
	# high to A). the sign must be valid in rA
	#
	my $low_value = abs($new_value) % (get_mix_max_word_value() + 1);
	$low_value *= -1 if $new_value < 0;
	my $high_value = int($new_value / (get_mix_max_word_value() + 1));

	$rX = value2word($low_value);
	$rA = value2word($high_value);
	
	# the sign of A matters, so copy it from X (where it 
	# will always be correct)
	$rA->[0] = $rX->[0];
	
	$time += 10;
}


# handler for the DIV instruction
#
sub op_div
{
	my @op_word = @_;
	my $op = $op_word[5];
	my ($M, $L, $R) = @{full_word_decode(@op_word)};
	
	my @V = @{word2v($mem[$M], $L, $R)};
	my $V_value = word2value(\@V);
	
	# rAX is treated as a 10 byte word with the sign of rA,
	# and is divided by V. The quotient goes to rA, the
	# remainder to rX. The sign of the result is the sign
	# of rA. 
	
	# division by 0 is undefined behavior
	#
	goto undefined_behav if ($V_value == 0);
	
	my $reg_value_high = word2value($rA);
	my $reg_value_low = word2value($rX);
	my $value = $reg_value_high * (get_mix_max_word_value() + 1) + $reg_value_low;
	
	# if reg_high is -0, we must still take the minus into account
	#
	$value *= -1 if ($rA->[0] eq "-" and $reg_value_high == 0);

	my $quotient = int($value / $V_value);

	$time += 12;

	# if the quotient is too large for rA, it's undefined behavior
	#
	goto undefined_behav if ($quotient > get_mix_max_word_value());
	
	my $remainder = $value % $V_value;
	$rA = value2word($quotient);
	$rX = value2word($remainder);
	
	return;
	
undefined_behav:
	$rA = empty_word();
	$rX = empty_word();		
	$f_overflow = 0;
	
	return;
}


# handler for all ENT, ENN, INC and DEC instructions
#
sub op_addr_transfer
{
	my @op_word = @_;
	my $op = $op_word[5];
	
	# here we don't need the contents of $mem[$M], we
	# only need the value of $M itself
	my $M = get_effective_address(@op_word);
	my $F = $op_word[4];
	my $value = word2value(\@op_word);
	
	# ENT instructions
	if ($F == 2)
	{
		# ENTA
		if ($op == 48)
		{
			$rA = value2word($M);
		}
		# ENTX
		elsif ($op == 55)
		{
			$rX = value2word($M);
		}
		# ENTi
		elsif ($op > 48 and $op < 55)
		{
			$rI[$op - 48] = value2word($M);
		}
		else
		{
			internal_error("illegal op_addr_transfer op $op");
		}
	}
	# ENN instructions
	elsif ($F == 3)
	{
		# ENNA
		if ($op == 48)
		{
			$rA = value2word(-$M);
		}
		# ENNX
		elsif ($op == 55)
		{
			$rX = value2word(-$M);
		}
		# ENNi
		elsif ($op > 48 and $op < 55)
		{
			$rI[$op - 48] = value2word(-$M);
		}
		else
		{
			internal_error("illegal op_addr_transfer op $op");
		}
	}
	# INC instructions
	elsif ($F == 0)
	{
		my $total;
		
		# INCA
		if ($op == 48)
		{
			$total = $M + word2value($rA);
			$rA = value2word($total);
		}
		# INCX
		elsif ($op == 55)
		{
			$total = $M + word2value($rX);
			$rX = value2word($total);
		}
		# INCi
		elsif ($op > 48 and $op < 55)
		{
			$total = $M + word2value($rI[$op - 48]);
			$rI[$op - 48] = value2word($total);
		}
		else
		{
			internal_error("illegal op_addr_transfer op $op");
		}
		
		$f_overflow = 1 
			if ($total > get_mix_max_word_value() or $total < -get_mix_max_word_value());
	}
	# DEC instructions
	elsif ($F == 1)
	{
		my $total;
		
		# DECA
		if ($op == 48)
		{
			$total = -$M + word2value($rA);
			$rA = value2word($total);
		}
		# DECX
		elsif ($op == 55)
		{
			$total = -$M + word2value($rX);
			$rX = value2word($total);
		}
		# DECi
		elsif ($op > 48 and $op < 55)
		{
			$total = -$M + word2value($rI[$op - 48]);
			$rI[$op - 48] = value2word($total);
		}
		else
		{
			internal_error("illegal op_addr_transfer op $op");
		}
		
		$f_overflow = 1 
			if ($total > get_mix_max_word_value() or $total < -get_mix_max_word_value());
	}
	else
	{
		runtime_error("illegal fspec $F for opcode $op");
	}	
	
	$time += 1;
}


# handler for all compare instructions:
# CMPA, CMPX, CMPi
#
sub op_cmp
{
	my @op_word = @_;
	my $op = $op_word[5];
	my ($M, $L, $R) = @{full_word_decode(@op_word)};
	my @V = @{word2v($mem[$M], $L, $R)};
	my $V_value = word2value(\@V);
	
	my @reg_word;
	
	# CMPA
	if ($op == 56)
	{
		@reg_word = @{$rA}[$L .. $R];
	}
	# CMPX
	elsif ($op == 63)
	{
		@reg_word = @{$rX}[$L .. $R];
	}
	# CMPi
	elsif ($op > 56 or $op < 63)
	{
		@reg_word = @{$rI[$op - 56]}[$L .. $R];
	}
	else
	{
		internal_error("illegal op_cmp op $op");
	}
	
	# if the sign wasn't included in the comparison, it is
	# assumed to be positive
	unshift(@reg_word, "+") if ($L > 0);
	
	my $reg_value = word2value(\@reg_word);
	
	# any positive value in the flag counts as GREATER, and 
	# any negative counts as LESS, so we can just subtract
	#
	$f_comparison = $reg_value - $V_value;
	
	$time += 1;
}	


# This function is called when a jump is actually executed.
#
sub execute_jump
{
	my $to = $_[0];
	
	# rJ holds the next location we'd be in w/o if the jump
	# weren't executed
	$rJ = value2word($lc + 1);
	
	# jump !
	# the simulator increases lc after each instruction, hence the -1
	#
	$lc = $to - 1;
}


# handler for all jump instructions
#
sub op_jump
{
	my @op_word = @_;
	my $op = $op_word[5];
	
	# here we don't need the contents of $mem[$M], we
	# only need the value of $M itself
	my $M = get_effective_address(@op_word);
	my $F = $op_word[4];

	if ($op == 39)
	{
		if ($F < 0 or $F > 9)
		{
			runtime_error("illegal fspec $F for opcode $op");
		}

		# JSJ - unconditional, but rJ is not modified (thus we
		# don't use execute_jump)
		#
		$lc = $M - 1 if ($F == 1);
		
							# JMP - unconditional
		execute_jump($M) if ($F == 0) or
							# JOV - if overflow is set
							($F == 2 and $f_overflow == 1) or
							# JNOV - if overflow isn't set
							($F == 3 and $f_overflow == 0) or
							# JL - if LESS
							($F == 4 and $f_comparison < 0) or
							# JE - if EQUAL
							($F == 5 and $f_comparison == 0) or
							# JG - if GREATER
							($F == 6 and $f_comparison > 0) or
							# JGE - if not less
							($F == 7 and $f_comparison >= 0) or
							# JNE - if not EQUAL
							($F == 8 and $f_comparison != 0) or
							# JLE - if not GREATER
							($F == 9 and $f_comparison <= 0);
	}
	elsif ($op >= 40 and $op <= 47)
	{
		if ($F < 0 or $F > 5)
		{
			runtime_error("illegal fspec $F for opcode $op");
		}
		
		# there is a whole bunch (48 !) of instructions that are very much alike,
		# so some means are taken to lessen the burden:
		#
		# - first the register value is fetched (it depends only on OP)
		# - then, perform jump if the condition holds(it depends only on F)
		#
		my $reg_value;
		
		# rA
		if ($op == 40)
		{
			$reg_value = word2value($rA);
		}
		# rX
		elsif ($op == 47)
		{
			$reg_value = word2value($rX);
		}
		# rI
		else
		{
			$reg_value = word2value($rI[$op - 40]);
		}
							# J{A,X,i}N
		execute_jump($M) if ($F == 0 and $reg_value < 0) or
							# J{A,X,i}Z
							($F == 1 and $reg_value == 0) or
							# J{A,X,i}P
							($F == 2 and $reg_value > 0) or
							# J{A,X,i}NN
							($F == 3 and $reg_value >= 0) or
							# J{A,X,i}NZ
							($F == 4 and $reg_value != 0) or
							# J{A,X,i}NP
							($F == 5 and $reg_value <= 0);
	}
	elsif ($op == 34) # JBUS
	{
		# for now, all devices are implemented as files with immediate 
		# operations, so they are never busy
	}
	elsif ($op == 38) # JRED
	{
		# for now, all devices are implemented as files with immediate 
		# operations, so they are never busy
		#
		execute_jump($M);
	}
	else
	{
		internal_error("illegal op_jump op $op");
	}
	
	$time += 1;
}


# Given a byte, converts it to a digit it represents.
# (modulo 10, so 11, 21, 31 ... all map to 1, as defined in Knuth)
#
sub byte2digit
{
	my $byte = $_[0];
	validate_byte($byte);
	
	return $byte % 10;
}


# handler for NUM and CHAR
#
sub op_conv
{
	my @op_word = @_;
	my $F = $op_word[4];
	my $op = $op_word[5];

	# NUM
	if ($F == 0)
	{
		my $value = "";
		
		# start with rA, because we append to the right
		for (my $i = 1; $i <= 5; ++$i)
		{
			my $byte = $rA->[$i];
			my $digit = byte2digit($byte);
			
			$value .= $digit;
		}
		
		# now append rX's bytes
		for (my $i = 1; $i <= 5; ++$i)
		{
			my $byte = $rX->[$i];
			my $digit = byte2digit($byte);
			
			$value .= $digit;
		}
		
		$f_overflow = 1 
			if ($value > get_mix_max_word_value() or $value < -get_mix_max_word_value());
			
		my @bytes = @{value2word($value)};
		
		# the sign of rA mustn't change
		$bytes[0] = $rA->[0];
		$rA = \@bytes;
	}
	# CHAR
	elsif ($F == 1)
	{
		my $value = word2value($rA);
		
		# init with 0s ("30" is 0)
		my @bytes = (30) x 10;
		
		my $i = 9;
		while ($value > 0)
		{
			$bytes[$i] = ($value % 10) + 30;
			$value = int($value / 10);
			
			--$i;
		}
		
		# now @bytes contains 10 bytes that are to 
		# be entered into rX (the low 5) and rA (the
		# high 5). Signs of rA and rX aren't modified
		#
		$rX = [$rX->[0], @bytes[5 .. 9]];
		$rA = [$rA->[0], @bytes[0 .. 4]];		
	}
	else
	{
		runtime_error("illegal fspec $F for opcode $op");
	}
	
	$time += 10;
}


# handler for MOVE
#
sub op_move
{	
	my @op_word = @_;
	my $F = $op_word[4];
	my $source = get_effective_address(@op_word);	# $M is $source
	
	# we will copy $F words
	# source: starts at $M
	# destination: starts at [rI1]
	
	my $dest = word2value($rI[1]);
	
	for (my $i = 0; $i < $F; ++$i)
	{
		$mem[$dest + $i] = $mem[$source + $i];
		$time += 2;
	}
	
	$time += 1;
}


# Performs a shift of an array
# 
# Arguments: the array, how much to shift, direction
# The directions are: 0 - right, 1 - right circular, 
#                     2 - left, 3 - left circular
#
# returns the shifted array
#
sub do_shift
{
	my @arr = @{$_[0]};
	my $N = $_[1];				# how much to shift
	my $dir = $_[2];			# shift direction
	
	(defined($dir) and $dir >= 0 and $dir <= 3) 
		or internal_error("internal do_shift shift direction: $dir");
	
	# 1 shift of N is N shifts of 1
	foreach (1 .. $N)
	{
		# shifting right, so save and remove rightmost cell
		if ($dir < 2)
		{
			my $cell = pop @arr;
			
			# right
			if ($dir == 0)
			{
				unshift(@arr, 0);
			}
			# cirlular right
			else
			{
				unshift(@arr, $cell);
			}
		}
		# shifting left, so save and remove leftmost cell
		else 
		{
			my $cell = shift @arr;
			
			# left
			if ($dir == 2)
			{
				push(@arr, 0);
			}
			# circular left
			else
			{
				push(@arr, $cell);
			}
		}
	}
	
	return \@arr;
}


# handler for the shift instructions:
# SLA, SRA, SLAX, SRAX, SLC, SRC
#
sub op_shift
{
	my @op_word = @_;
	my $F = $op_word[4];
	my $op = $op_word[5];
	my $M = get_effective_address(@op_word);
	
	# SLA and SRA
	if ($F == 0 or $F == 1)
	{
		my @reg = @{$rA}[1 .. 5];		
		my $dir = $F == 0 ? 2 : 0;
		my @shifted = @{do_shift(\@reg, $M, $dir)};
		
		$rA = [$rA->[0], @shifted];
	}
	# SLAX, SRAX, SLC and SRC
	elsif ($F >=2 or $F <= 5)
	{
		my @reg = (@{$rA}[1 .. 5], @{$rX}[1 .. 5]);
		
		# map $F to the shift direction given to do_shift
		my %dir_map = (2 => 2, 3 => 0, 4 => 3, 5 => 1);
		my $dir = $dir_map{$F};
		
		my @shifted = @{do_shift(\@reg, $M, $dir)};
		
		$rA = [$rA->[0], @shifted[0 .. 4]];
		$rX = [$rX->[0], @shifted[5 .. 9]];
	}
	else
	{
		runtime_error("illegal fspec $F for opcode $op");
	}

	$time += 2;
}


sub is_input_device
{
	my ($devref) = @_;
	return ($devref->{io_type} =~ /i/) ? 1 : 0;
}


sub is_output_device
{
	my ($devref) = @_;
	return ($devref->{io_type} =~ /o/) ? 1 : 0;
}


sub is_binary_device
{
	my ($devref) = @_;
	return ($devref->{io_type} =~ /b/) ? 1 : 0;
}


sub open_binary_device
{
	my ($devref) = @_;
	
	# Deal only with binary devices that hasn't been opened yet
	# A binary device is considered "open" when it's "data" 
	# member is defined
	#
	return if (	not is_binary_device($devref) or 
				(exists($devref->{data}) and defined($devref->{data})));
	
	my $data = {};
	
	# If the device file exists, read it.
	#
	# The format is very simple - a block number is followed by the
	# words of this block.
	# 
	# <block num>
	# <word>
	# <word>
	# ... total of block_size words
	# <block num>
	# <word>
	# ... etc
	#
	# The device file should be only written and read by the simulator;
	# mistakes in its format may lead to serious errors.
	#
	my $fh = $devref->{handle};
	
	if (open($fh, $devref->{filename}))
	{
		while (my $line = <$fh>)
		{
			chomp $line;
			
			if ($line =~ /^(\d+)$/)
			{
				# make sure that the block number is legal
				#
				my $block_num = $1;
				next if $block_num < 0 or 
						$block_num >= 2 ** (2 * get_mix_bits_in_byte()) - 1;
				
				my @empty_block = (empty_word()) x $devref->{block_size};
				$data->{$block_num} = \@empty_block;
				
				for (my $word_n = 0; $word_n < $devref->{block_size}; ++$word_n)
				{
					my $line = <$fh>;
					last unless $line;
					chomp $line;
					
					if ($line =~ /^\s* ([+-]) \s+ (\d\d?) \s+ (\d\d?) \s+ (\d\d?) \s+ (\d\d?) \s+ (\d\d?)$/x)
					{
						$data->{$block_num}->[$word_n] = [$1, $2, $3, $4, $5, $6];
					}
				}
			}
		}
	}
	
	$devref->{data} = $data;
}


# handler for the OUT instruction
#
sub op_output
{
	my @op_word = @_;
	my $F = $op_word[4];
	my $M = get_effective_address(@op_word);

	($F <= 20) or runtime_error("bad device number for OUT");

	my $devref = $io_device[$F];

	is_output_device($devref) or runtime_error("device $F is not for output with OUT");
	(address_is_legal($M) and address_is_legal($M + $devref->{block_size})) 
		or runtime_error("OUT on block that is partly not in memory");
		
	# file handle to the device
	#
	my $fh;
		
	# terminal 
	#
	if ($F == 19)
	{
		$fh = *STDOUT;
	}
	elsif (is_binary_device($devref))
	{
		open_binary_device($devref);
	}
	# if the filehandle to this device still doesn't exist, open it
	#
	elsif (not exists($devref->{handle}))
	{
		open($fh, ">$devref->{filename}") or runtime_error("unable to write to device $F");
		$devref->{handle} = $fh;
	}
	else
	{
		$fh = $devref->{handle};
	}
	
	if (is_binary_device($devref))
	{
		my $block_n = current_device_block_n();
		$devref->{data}->{$block_n} = [];
		
		for (my $memptr = $M; $memptr < $M + $devref->{block_size}; ++$memptr)
		{
			$devref->{data}->{$block_n}->[$memptr - $M] = $mem[$memptr];
		}
	}
	else
	{
		for (my $memptr = $M; $memptr < $M + $devref->{block_size}; ++$memptr)
		{
			for (my $i = 1; $i <= 5; ++$i)
			{
				print $fh byte2char($mem[$memptr]->[$i]);
			}
		}
	}
	
	print $fh "\n" if (not is_binary_device($devref));

	$time += 1;
}


# The value of the lowest two bytes of rX (TAOCP 1.3.1, "input
# output operators")
#
sub current_device_block_n
{
	my $ptr_word = ["+", 0, 0, 0, $rX->[4], $rX->[5]];
	my $val = word2value($ptr_word);
	
	return $val;
}


# handler for the IN instruction
#
sub op_input
{
	my @op_word = @_;
	my $F = $op_word[4];
	my $M = get_effective_address(@op_word);
	
	($F <= 20) or runtime_error("bad device number for OUT");

	my $devref = $io_device[$F];

	is_input_device($devref) or runtime_error("device $F is not for input with IN");
	(address_is_legal($M) and address_is_legal($M + $devref->{block_size})) 
		or runtime_error("IN to block that is partly not in memory");

	# file handle to the device
	#
	my $fh;
		
	# terminal 
	#
	if ($F == 19)
	{
		$fh = *STDIN;
	}
	elsif (is_binary_device($devref))
	{
		open_binary_device($devref);
	}
	# if the filehandle to this device still doesn't exist, open it
	#
	elsif (not exists($devref->{handle}))
	{
		open($fh, "$devref->{filename}") or runtime_error("unable to read device $F");
		$devref->{handle} = $fh;
	}
	else
	{
		$fh = $devref->{handle};
	}
	
	# Now do the actual input 
	# 
	if (is_binary_device($devref))
	{
		my $block_n = current_device_block_n();
		
		for (my $i = 0; $i < $devref->{block_size}; ++$i)
		{
			if (exists $devref->{data}->{$block_n})
			{
				$mem[$M + $i] = $devref->{data}->{$block_n}->[$i];
			}
			else
			{
				$mem[$M + $i] = empty_word();
			}
		}
	}
	else
	{
		# Read the line from the device, and make sure it contains
		# exactly block_size words.
		#
		my $line = <$fh>;
		
		if (not $line)
		{
			return;
		}
		
		chomp $line;
		my $n_bytes_expected = $devref->{block_size} * 5;
		
		if (length($line) > $n_bytes_expected)
		{
			$line = substr($line, 0, $n_bytes_expected);
		}
		elsif (length($line) < $n_bytes_expected)
		{
			$line .= " " x ($n_bytes_expected - length($line));
		}
		
		# Now convert 5-tuples of characters into MIX words and 
		# write them to memory.
		#
		for (my $i = 0; $i < length($line); $i += 5)
		{
			my $word = ["+"];
			
			for (my $j = 0; $j < 5; ++$j)
			{
				my $c = substr($line, $i + $j, 1);
				
				if (defined char2byte($c))
				{
					push(@$word, char2byte($c));
				}
				else
				{
					runtime_error("Unknown character in device $F\nLine: $line\nPos: " . ($i + $j));
				}
			}
			
			$mem[$M + $i / 5] = $word;
		}
	}
	
	$time += 1;
}


# handler for the IOC instruction
#
sub op_ioc
{
	my @op_word = @_;
	my $F = $op_word[4];

	$time += 1;
}


1;
