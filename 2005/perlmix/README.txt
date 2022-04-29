##
#
# PerlMIX - the MIX assembler/simulator in Perl
#
#   (C) 2005 by Eli Benderksy
#   License: this code is in the public domain
#
##

PerlMIX is a complete implementation of the MIX computer and 
the MIXAL programming language described by Knuth in TAOCP.
It attempts to be as compatible as possible to the GNU MIX
Development Kit (MDK) by Jose Antonio Ortega Ruiz.

PerlMIX was developed and tested on Active Perl v5.8.4, Windows 2000
It requires an installation of the Parse::RecDescent module. This code
was developed in 2005 and may need a bit of massaging to run on modern
Perl installations.

What you'll find in the distribution:

MIX/ 		- PerlMIX modules (.pm) files. Read the documentation
			  on top of each module file for info.

hello.mixal	- A trivial "Hello world" program written in the MIXAL language

hello.pl 	- Assembles and simulates hello.mixal, should get you started
			  quickly with PerlMIX

test.pl		- A script for regression-testing of PerlMIX

test/		- Directory with regression test (see test.pl for info).	
			  Contains examples of more programs written in MIXAL, some
			  of them quite complex.
