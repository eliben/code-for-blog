##
#
# PerlMIX - the MIX assembler/simulator in Perl
#
#   by Eli Benderksy
#
##
#
# Runs some regression tests on PerlMIX.
# 
# Input: expects a directory "test/" divided internally as
# one directory per each test, which is a file ending with
# .mixal
#
# Output: each test prints OK to the terminal if it passed.
# If you don't see OK for some test, it probably failed.
#
# Take a look at the test/ directory structure and run 
# test.pl once to get a feel for how it works.
#
use warnings;
use strict;
use File::Find;
$|++;

use MIX::Asm;
use MIX::Sim;

find(\&test_dir, "test/");

sub test_dir
{
	my $filename = $_;
	
	if ($filename =~ /\.mixal$/)
	{
		init_assembler();
		my ($mref, $init_addr) = assemble_file("$filename");
		
		init_sim(-mem_ref => $mref, -init_addr => $init_addr);
		print "Test $File::Find::name: ";
		run_sim();
		print "\n";
	}
}
