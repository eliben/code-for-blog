##
#
# PerlMIX - the MIX assembler/simulator in Perl
#
#   by Eli Benderksy
#
##
#
# A simple example of a MIXAL program assembled and
# simulated by PerlMIX.
#
# Make sure that hello.mixal is in the same directory,
# and that the PerlMIX modules are placed correctly
# in the MIX/ directory - then run this example.
# It assembles hello.mixal into a memory map, and then
# simulates the memory map - which should print:
# MIXAL HELLO WORLD
#
use warnings;
use strict;
$|++;

use MIX::Asm;
use MIX::Sim;

print "<Assembling...>\n";
init_assembler();
my ($mref, $init_addr) = assemble_file('hello.mixal');

# Uncomment this if you want to see the memory map and initial
# address - the output of the assembler
#~ print "\n\n" . memory_dump($mref) . "\n\nStart: $init_addr\n\n";

print "<Simulating...>\n";
init_sim(-mem_ref => $mref, -init_addr => $init_addr, -device_dir => '');

# Uncomment this if you want an interactive/debugging session instead
#~ interactive_sim();

run_sim();

# Uncomment this if you want to see the memory/state of the MIX
# simulator after the simulation
#~ print "\n\nAFTER SIMULATION\n----------------\n\n" . memory_dump(get_mem_ref()) . "\n\n" . state_dump() . "\n\n";

