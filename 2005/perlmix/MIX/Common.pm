##
#
# PerlMIX - the MIX assembler/simulator in Perl
#
#   by Eli Benderksy
#
##
#
# Package MIX::Common
#
# Common utilities for the PerlMIX project.
# There is no point using the utilities from
# this package directly, unless you just want
# to play with PerlMIX-es low-level primitives.
#
package MIX::Common;

use warnings;
use strict;
use Carp;
require Exporter;

use vars qw(@ISA @EXPORT $VERSION @EXPORT_OK);

$VERSION 	= 1.0;
@ISA     	= qw(Exporter);
@EXPORT  	= qw(	get_mix_max_word_value get_mix_mem_size internal_error empty_word 
					word2value value2word range2f f2range byte2char char2byte memory_dump
					build_instr_word address_is_legal get_mix_bits_in_byte
				);
@EXPORT_OK 	= qw();

my $MIX_MEM_SIZE = 4000;

my $MIX_BITS_IN_BYTE = 6;

# the maximal value that can be stored in a word (5 bytes, 6 bits each).
#
my $MIX_MAX_WORD_VALUE = ((2 ** $MIX_BITS_IN_BYTE) ** 5 - 1);

# Map MIX byte values to characters (alphanumeric + punctuation and signs)
# There are 3 special characters:
# d - delta, s - sigma, g - gamma
#
my %byte2char_map = (
				0 	=> " ", 1	=> "A", 2 	=> "B", 3	=> "C",
				4	=> "D", 5 	=> "E", 6	=> "F", 7	=> "G",
				8	=> "H", 9	=> "I", 10	=> "d",	11	=> "J",
				12	=> "K", 13	=> "L", 14	=> "M",	15	=> "N",
				16	=> "O", 17	=> "P", 18	=> "Q",	19	=> "R",
				20	=> "s", 21	=> "g", 22	=> "S",	23	=> "T",
				24	=> "U", 25	=> "V", 26	=> "W",	27	=> "X",
				28	=> "Y", 29	=> "Z", 30	=> "0",	31	=> "1",
				32	=> "2", 33	=> "3", 34	=> "4",	35	=> "5",
				36	=> "6", 37	=> "7", 38	=> "8",	39	=> "9",
				40	=> ".", 41	=> ",", 42	=> "(",	43	=> ")",
				44	=> "+", 45	=> "-", 46	=> "*",	47	=> "/",
				48	=> "=", 49	=> '$', 50	=> "<",	51	=> ">",
				52	=> '@', 53	=> ";", 54	=> ":",	55	=> "'");	

my %char2byte_map = reverse(%byte2char_map);


sub byte2char
{
	my ($byte) = @_;
	
	return exists($byte2char_map{$byte}) ? $byte2char_map{$byte} : undef;
}


sub char2byte
{
	my ($char) = @_;
	
	return exists($char2byte_map{$char}) ? $char2byte_map{$char} : undef;
}


sub get_mix_mem_size
{
	return $MIX_MEM_SIZE;
}


sub get_mix_max_word_value
{
	return $MIX_MAX_WORD_VALUE;
}


sub get_mix_bits_in_byte
{
	return $MIX_BITS_IN_BYTE;
}


sub empty_word
{
	return ["+", 0, 0, 0, 0, 0];
}


sub internal_error
{
	my ($msg) = @_;
	
	confess("Internal error: $msg\n");
}


# Given an array of bytes (of any length), calculates
# the word it represents.
# 
# Note: The array's first elements are the sign and MSB
#
sub word2value
{
	my @bytes = @{$_[0]};
	my $exponent = 0;
	my $result = 0;
	my $sign = shift @bytes;

	validate_sign($sign);
	
	foreach (reverse(@bytes))
	{
		validate_byte($_);
	
		$result += $_ * (64 ** $exponent);
		
		++$exponent;
	}
	
	$result = -$result if ($sign eq "-");
	
	return $result;
}


# Given a value, calculates the MIX word that
# represents it
#
sub value2word
{
	my $sign = ($_[0] < 0) ? "-" : "+";
	my $value = abs($_[0]);
	
	$value %= get_mix_max_word_value() 
		if ($value > get_mix_max_word_value() or $value < -get_mix_max_word_value());
	
	my @bytes;
	
	for (my $exponent = 4; $exponent >= 0; --$exponent)
	{
		my $tmp = 64 ** $exponent;
		my $byte = int($value / (64 ** $exponent));
		$value %= $tmp;		
		
		push(@bytes, $byte);
	}
	
	unshift(@bytes, $sign);
	return \@bytes;
}


# Return encoding of the F field given a range
#
sub range2f
{
	my ($L, $R) = @_;
	validate_fspec($L, $R);
	
	return 8 * $L + $R;
}


# Return the range represented by an F field
#
sub f2range
{
	my $F = $_[0];	
	my ($L, $R) = (int($F / 8), $F % 8);	
	validate_fspec($L, $R);
	
	return [$L, $R];
}


sub validate_fspec
{
	my ($L, $R) = @_;
	
	die("illegal fspec range: ($L:$R)\n")
		if ($L < 0 or $L > 5 or $R < 0 or $R > 5 or $R < $L);
}


sub validate_byte
{
	my $byte_val = $_[0];
	
	 die("illegal byte value: $byte_val\n")
		if ($byte_val < 0 or $byte_val > 63);
}


sub validate_address
{
	my $address = $_[0];
	
	die("illegal address: $address\n")
		if (not address_is_legal($address));
}


sub address_is_legal
{
	my $address = $_[0];
	
	return ($address >= 0 and $address < get_mix_mem_size());
}


sub validate_sign
{
	my $sign = $_[0];
	
	confess("illegal sign: $sign\n")
		if ($sign ne "+" and $sign ne "-");
}


sub words_equalp
{
	my @word1 = @{$_[0]};
	my @word2 = @{$_[1]};
	
	# compare signs
	return 0 if $word1[0] ne $word2[0];
		
	# compare everything else
	foreach my $i (1 .. $#word1)
	{
		return 0 if $word1[$i] != $word2[$i];
	}
		
	return 1;
}


# Given A (signed), F, I and C, builds an instruction word
#
sub build_instr_word
{
	my ($A, $F, $I, $C) = @_;
	my $A_word = value2word($A);
	
	my @word = ($A_word->[0], $A_word->[4], $A_word->[5], $I, $F, $C);
	
	return \@word;
}


# Returns a memory dump - contents of all non-empty memory 
# cells.
#
sub memory_dump
{
	my ($mem) = @_;
	my $dump_str = "";
	
	foreach (my $addr = 0; $addr < get_mix_mem_size(); ++$addr)
	{
		if (not defined $mem->[$addr])
		{
			confess "undefined data at memory address $addr\n";
		}
		
		my @word = @{$mem->[$addr]};
		
		next if words_equalp(\@word, empty_word());
		
		$dump_str .= sprintf("%4s : %2s %2s %2s %2s %2s %2s\n", $addr, @word);
	}
	
	return $dump_str;
}


1;
