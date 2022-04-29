##
#
# PerlMIX - the MIX assembler/simulator in Perl
#
#   by Eli Benderksy
#
##
#
# Package MIX::Asm
#
# Implementation of MIXAL - the MIX assembler.
# Understands plain text files in MIXAL language,
# and compiles them into MIX machine code suitable
# to be run by the MIX simulator (MIX::Sim).
#
# Exported functions:
#
# init_assembler 	- resets the assembler
# assemble_file 	- assembles a file into a MIX memory map
#
# Usage:
#
# use MIX::Asm;
#
# init_assembler();
# my ($mref, $init_addr) = assemble_file("some_file");
#
# You end up with $mref - a MIX memory map, and $init_addr - the
# initial address from which the simulator should start executing.
#
# To simulate what you got with MIX::Sim:
#
# use MIX::Sim;
#
# init_sim(-mem_ref => $mref, -init_addr => $init_addr,
#          -device_dir => 'directory with MIX devices');
#
# run_sim();
#
# Please see MIX::Sim for more details.
#
# Interface: the interface of MIX::Asm is not object oriented, and
# is not state-less, there will be problems running it in several
# threads (not that you have any reason to do it...)
#
# Implementation note: MIX::Asm is a complete compiler, it has
# an analyzing front-end (MIXAL language -> internal syntax/semantic
# data) and a synthesizing back-end (internal data -> MIX machine
# language words).
#
# The front-end is implemented on the basis of Parse::RecDescent
# (see the grammar below). Note that Parse::RecDescent's error
# messages are a bit cryptic at times. I tried making the Parse
# errors as understandable as possible, but sometimes they're
# non-intuitive.
#
package MIX::Asm;

use warnings;
use strict;
use Carp;

use Data::Dumper;
use Parse::RecDescent;
use MIX::Common;
require Exporter;

use vars qw(@ISA @EXPORT $VERSION @EXPORT_OK);

$VERSION 	= 1.0;
@ISA     	= qw(Exporter);
@EXPORT  	= qw(init_assembler assemble_file);



					##-----------##
#####################=- GRAMMAR -=#####################
					##-----------##

my $grammar =
q {
		all_file:
				/\s*/ <skip:''> mixal_line(s) /^\s*$/
				{
					$return = [];

					# keep only code lines (lines that aren't whitespace
					# aren't comments)
					#
					map {push(@{$return}, $_) unless $_ == -1} @{$item[3]};
				}

		# A line is either and optional label + code + optional remark, or an empty line
		# (whitespace only), or a comment beginning with an asterisk.
		# Note: this would be nicer with label(?) but ? is greedy and we must try to
		# match a command/directive before a label.
		#
		mixal_line:

				/\s*/ mixal_code remark(?) end_of_line
				{
					$item[2]->{label} = undef;
					$item[2]->{line_n} = $thisline - 1;
					$item[2];
				}

			|	/\s*/ label mixal_code remark(?) end_of_line
				{
					$item[3]->{label} = $item[2];
					$item[3]->{line_n} = $thisline - 1;
					$item[3];
				}

			|   /^\*[^\n]*/ end_of_line
				{-1}

			|	/\s*\n/
				{-1}

			|	end_of_line
				{-1}

			|	<error>

		mixal_code:
				symbolic_mix_op
				{
					{
						type 	=> "op",
						cmd		=> $item[1],
						arg		=> undef
					};
				}

			|	symbolic_mix_op_with_address whitespace address
				{
					{
						type 	=> "op",
						cmd		=> $item[1],
						arg		=> $item[3]
					};
				}

			|	mixal_directive whitespace W_value
				{
					{
						type 	=> "directive",
						cmd 	=> $item[1],
						arg 	=> $item[3]
					};
				}

			|	alf_directive whitespace alf_string
				{
					{
						type 	=> "directive",
						cmd		=> $item[1],
						arg 	=> $item[3]
					};
				}

			|	<error>

		label:
				SYMBOL whitespace
				{
					$item[1];
				}

		address:
				"=" W_value "="
				{
					$item[2];
				}

			|	A_part index_part(?) F_part(?)
				{
					{
						'A' => $item[1],
						'I' => $item[2]->[0],
						'F' => $item[3]->[0]
					}
				}

		atom:
				'*'
			|	SYMBOL
			|	NUMBER

		unaried_atom:
				'+' atom
				{
					[$item[1], $item[2]];
				}

			|	'-' atom
				{
					[$item[1], $item[2]];
				}

			|	atom
				{
					[$item[1]];
				}

		# expr is convenient to define with leftop, but the $item
		# returning should be smarter, so flat_expr is a wrapper
		# that flattens the expression into a single array ref
		#
		expr:
				<leftop: unaried_atom BINOP unaried_atom>

		flat_expr:
				expr
				{
					map {push(@{$return}, ref($_) ? @{$_} : $_)} @{$item[1]};
				}

		A_part:
				flat_expr
				{
					$item[1];
				}

		index_part:
				',' flat_expr
				{
					$item[2];
				}

		F_part:
				'(' flat_expr ')'
				{
					$item[2];
				}

		expr_with_opt_F:
				flat_expr F_part(?)
				{
					{
						'A' => $item[1],
						'F' => (defined($item[2]->[0]) ? @{$item[2]} : undef)
					}
				}

		W_value:
				<leftop: expr_with_opt_F ',' expr_with_opt_F>

		symbolic_mix_op_with_address:
				/LD[1-6AX]N?/ 	| /ST[1-6AXJZ]/ | /EN[NT][1-6AX]/	|
				/INC[1-6AX]/ 	| /DEC[1-6AX]/ 	| /CMP[1-6AX]/ 		|
				"ADD" 	| "SUB" | "MUL" | "DIV" | "MOVE" |
				"IN" 	| "OUT" | "IOC" |
				/J(NOV|BUS|RED|MP|SJ|OV|GE|NE|LE|L|E|G)/ |
				/J[1-6AX](NN|NZ|NP|N|P|Z)/ |
				/S(L|R)(AX|A|C)/

		symbolic_mix_op:
				"HLT" | "NOP" | "NUM" | "CHAR"

		mixal_directive:
				"EQU" | "ORIG" | "CON" | "END"

		alf_directive:
				"ALF"

		# a remark is separated from the code with whitespace
		#
		remark:
				whitespace /[^\n]*/

		alf_string:
				/\"[^\"]{5}\"/

		BINOP:
				'//' | '/' | '+' | '-' | '*' | ':'

		# A symbol is a alphameric (caps only) string containting at least one letter
		#
		SYMBOL:
				/\w*[A-Z]\w*/

		NUMBER:
				/\d+/

		whitespace:
				/[ \t]+/

		end_of_line:
				"\n"
};

					##------------------##
#####################=- END OF GRAMMAR -=#####################
					##------------------##


# Maps symbolic command names (as parsed from the mixal source) to [opcode, F]
# opcode [0] is always defined. F [1] may be undefined, which means
# the F_spec that came with the command will be used as F.
# This mapping directly follows Knuth's description in TAOCP 1.3.1
#
# Some ops contain an "i". These will be expanded to 6 ops, for 1-6
# instead of "i", and OP+i instead of OP (following Knuth's convention)
#
my %opcode_map =
(
	'LDA'	=> [8],
	'LDX'	=> [15],
	'LDi'	=> [8],
	'LDAN'	=> [16],
	'LDXN'	=> [23],
	'LDiN'	=> [16],
	'STA'	=> [24],
	'STX'	=> [31],
	'STi'	=> [24],
	'STJ'	=> [32],
	'STZ'	=> [33],
	'ADD'	=> [1],
	'SUB'	=> [2],
	'MUL'	=> [3],
	'DIV'	=> [4],
	'ENTA'	=> [48, 2],
	'ENTX'	=> [55, 2],
	'ENTi'	=> [48, 2],
	'ENNA'	=> [48, 3],
	'ENNX'	=> [55, 3],
	'ENNi'	=> [48, 3],
	'INCA'	=> [48, 0],
	'INCX'	=> [55, 0],
	'INCi'	=> [48, 0],
	'DECA'	=> [48, 1],
	'DECX'	=> [55, 1],
	'DECi'	=> [48, 1],
	'CMPA'	=> [56],
	'CMPX'	=> [63],
	'CMPi'	=> [56],
	'JMP'	=> [39, 0],
	'JSJ'	=> [39, 1],
	'JOV'	=> [39, 2],
	'JNOV'	=> [39, 3],
	'JL'	=> [39, 4],
	'JE'	=> [39, 5],
	'JG'	=> [39, 6],
	'JGE'	=> [39, 7],
	'JNE'	=> [39, 8],
	'JLE'	=> [39, 9],
	'JAN'	=> [40, 0],
	'JAZ'	=> [40, 1],
	'JAP'	=> [40, 2],
	'JANN'	=> [40, 3],
	'JANZ'	=> [40, 4],
	'JANP'	=> [40, 5],
	'JXN'	=> [47, 0],
	'JXZ'	=> [47, 1],
	'JXP'	=> [47, 2],
	'JXNN'	=> [47, 3],
	'JXNZ'	=> [47, 4],
	'JXNP'	=> [47, 5],
	'JiN'	=> [40, 0],
	'JiZ'	=> [40, 1],
	'JiP'	=> [40, 2],
	'JiNN'	=> [40, 3],
	'JiNZ'	=> [40, 4],
	'JiNP'	=> [40, 5],
	'MOVE'	=> [7],
	'SLA'	=> [6, 0],
	'SRA'	=> [6, 1],
	'SLAX'	=> [6, 2],
	'SRAX'	=> [6, 3],
	'SLC'	=> [6, 4],
	'SRC'	=> [6, 5],
	'NOP'	=> [0, 0],
	'HLT'	=> [5, 2],
	'IN'	=> [36],
	'OUT'	=> [37],
	'IOC'	=> [35],
	'JRED'	=> [38],
	'JBUS'	=> [34],
	'NUM'	=> [5, 0],
	'CHAR'	=> [5, 1]
);


# Expand all the commands containing "i" (see comment above %opcode_map)
#
foreach my $key (keys %opcode_map)
{
	if ($key =~ /i/)
	{
		my $op = $opcode_map{$key};

		foreach my $i (1..6)
		{
			my $new_key = $key;
			$new_key =~ s/i/$i/;

			my @new_op = @$op;
			$new_op[0] += $i;
			$opcode_map{$new_key} = \@new_op;
		}

		delete $opcode_map{$key};
	}
}

# The memory image and first execution address will be
# filled in by the assembler when it finishes its job
#
my @mem = ();
my $first_execution_addr = 0;

# Points to the next memory location to be assembled
#
my $mem_ptr = 0;

# The assembler computes label values during a first pass where
# it makes sense of addresses and EQU directives. Later, when
# the assembly proceeds, the table is full with values for all labels.
#
# The key is a label name and the value is a [value, line_n] pair, which
# means that the label was set to /value/ at line /line_n/.
#
# Local (\dH) symbols get a special treatment: for those, the value is
# an array of [value, line_n, value, line_n, ...] with all the values
# set to that symbol and the lines where those were set, most-recent
# first.
#
my %label_table = ();

# the first pass detects where the first END directive is found - undefined
# symbols and literal constants are inserted here.
#
my $sym_insert_addr = 0;

# init parser
#
$::RD_WARN = 1;
$::RD_HINT = 1;
my $mixal_parser =  new Parse::RecDescent($grammar) or die "Internal error: Bad grammar\n";


# Initializes the assembler by cleaning the memory and symbol tabless
#
sub init_assembler
{
	@mem = ();
	push(@mem, empty_word()) foreach (1 .. get_mix_mem_size());
	$mem_ptr = 0;
	%label_table = ();
	$sym_insert_addr = 0;
}


# Assembles a MIXAL file into the current memory map. May be called
# on multiple files to assemble them into a single memory space.
#
# Arguments: $filename to assemble
# Returns: ($memref, $start_addr) - reference to the memory map and an
#          address to start execution from.
#
sub assemble_file
{
	my ($filename) = @_;

	open(IN_FILE, $filename) or die "Assembler unable to open $filename: $!\n";
	my $text = join('', <IN_FILE>);
	$text .= "\n";

	my $parsed_data = $mixal_parser->all_file($text);
	defined($parsed_data) or die "Assembler parse error !\n";

	first_pass($parsed_data);

	# restart memory counter - we start assembling from 0
	#
	$mem_ptr = 0;

	foreach my $line (@$parsed_data)
	{
		my $end_reached = assemble_line($line);

		last if $end_reached;
	}

	return (\@mem, $first_execution_addr);
}


# Calculates the values of all labels, to allow future references
# in MIXAL code
#
# Keeps count of the current assembly address and handles only the
# directives relevant to label values: EQU and ORIG
#
sub first_pass
{
	my ($parsed_data) = @_;
	my $end_loop = 0;

	first_pass_loop: foreach my $line (@$parsed_data)
	{
		last if $end_loop; # using a flag because of the "eval"

		# catch problems and report them uniformly
		#
		eval
		{
			# We are interested in labeled lines - these labels
			# can be set through EQU or just with $mem_ptr in non EQU
			# lines.
			#
			if (defined($line->{label}))
			{
				# forward/back local references must not
				# appear as labels
				#
				if ($line->{label} =~ /^\d[FB]$/)
				{
					die("$line->{label} must not appear as a label\n");
				}

				if ($line->{cmd} eq "EQU")
				{
					my $w_val = calc_w_value($line->{arg});
					set_label_value($line, $w_val);
				}
				else
				{
					set_label_value($line, $mem_ptr);
				}
			}

			# To know where to advance $mem_ptr to we have to pay
			# special attention to ORIG lines (that set it) and
			# to EQU lines (that don't change it)
			#
			if ($line->{cmd} eq "ORIG")
			{
				$mem_ptr = calc_w_value($line->{arg});
			}
			elsif ($line->{cmd} eq "EQU")
			{
			}
			elsif ($line->{cmd} eq "END")
			{
				$sym_insert_addr = $mem_ptr;
				$end_loop = 1;
			}
			else
			{
				$mem_ptr++;
			}
		};
		if ($@) {assembly_error($@, $line)};
	}
}


# Calculates the numeric value of a W expression
#
sub calc_w_value
{
	my ($w_expr) = @_;
	my $w = empty_word();

	foreach my $af_part (@$w_expr)
	{
		my $A = calc_expr_value($af_part->{'A'});
		my $range = defined($af_part->{'F'}) ? f2range(calc_expr_value($af_part->{'F'})) : [0, 5];

		build_w($w, $range, value2word($A));
	}

	return word2value($w);
}


# Arguments:
# $w - the target word
# $f - a range
# $inp - input word
#
# (all arguments are assumed to be legal and well formed)
#
# Puts into range $f in $w the right amount of words (starting
# with the rightmost) from $inp.
#
sub build_w
{
	my ($w, $f, $inp) = @_;
	my ($L, $R) = @$f;
	my $len = $R - $L;

	@$w[$L .. $R] = @$inp[5 - $len .. 5];

	# if the range includes the sign byte, the sign of $w is set
	# to be the same as the sign of $inp
	#
	if ($L == 0)
	{
		$w->[0] = $inp->[0];
	}
}


# Calculates the numeric value of an expression
#
sub calc_expr_value
{
	my @expr = @{$_[0]};

	my $result = fetch_expr_operand(\@expr);

	while (my $binop = shift @expr)
	{
		my $rightop = fetch_expr_operand(\@expr);

		if ($binop eq "+")
		{
			$result += $rightop;
		}
		elsif ($binop eq "-")
		{
			$result -= $rightop;
		}
		elsif ($binop eq "*")
		{
			$result *= $rightop;
		}
		elsif ($binop eq "/")
		{
			$result = int($result / $rightop);
		}
		elsif ($binop eq "//")
		{
			# zzz: *may* be overflow if $rightop is considerably
			# larger than $result. should probably use some bigint (?!)
			#
			$result = int(($result / $rightop) * get_mix_max_word_value());
		}
		elsif ($binop eq ":")
		{
			$result = 8 * $result + $rightop;
		}
		else
		{
			die "unknown binop $binop";
		}
	}

	return $result;
}


sub fetch_expr_operand
{
	my ($expr) = @_;
	(defined $expr->[0]) or die "expr operand missing";

	my $result = 0;
	my $negate = 0;

	if ($expr->[0] eq "+")
	{
		shift @$expr;
	}
	elsif ($expr->[0] eq "-")
	{
		$negate = 1;
		shift @$expr;
	}

	($expr->[0] =~ /^\dH$/) and die "H local symbol in expression: $expr->[0]\n";
	($expr->[0] =~ /^\dF$/) and die "future reference in expression: $expr->[0]\n";

	if (is_symbol($expr->[0]))
	{
		# back references (\dB) refer to \dH labels with the same \d
		# Note that values for local labels are stored "most recent first",
		# so we can just fetch the first value (during first pass !)
		#
		my $real_label = ($expr->[0] =~ /^(\d)B$/) ? "$1H" : $expr->[0];

		(exists($label_table{$real_label})) or die "symbol $real_label in expression wasn't previously defined\n";
		$result = $label_table{$real_label}->[0];
	}
	elsif (is_number($expr->[0]))
	{
		$result = $expr->[0];
	}
	elsif ($expr->[0] eq "*")
	{
		$result = $mem_ptr;
	}
	else
	{
		die "token $expr->[0] is an illegal expression operand\n";
	}

	shift @$expr;

	return $negate ? -$result : $result;
}


sub is_symbol
{
	my ($token) = @_;

	return ($token  =~ /^\w*[A-Z]\w*$/);
}


sub is_number
{
	my ($token) = @_;

	return ($token =~ /^\d+$/);
}


sub assemble_line
{
	my ($code_line) = @_;
	my $end_reached = 0;

	# Each parsed code line is a hash reference, with the following
	# fields:
	#
	#  type - op (MIX symbolic operation), directive (MIXAL directive)
	#
	#  cmd - command name
	#
	#  argument - address, W_value, alf_string or none, depending on type
	#             and cmd
	#
	#  line_n - line number in the original MIXAL file
	#
	if ($code_line->{type} eq "directive")
	{
		$end_reached = assemble_directive($code_line);
	}
	elsif ($code_line->{type} eq "op")
	{
		assemble_op($code_line);
	}
	else
	{
		internal_error("illegal code line type $code_line->{type}\n");
	}

	return $end_reached;
}


sub assemble_op
{
	my ($code_line) = @_;

	eval
	{
		# First some preprocessing is done, to sort out literal
		# constants, undefined and local symbols
		#
		# $real_arg will be modified throughout this stage and
		# eventually will contain the processed argument of the op
		#
		my $real_arg = $code_line->{arg};

		if (not defined($real_arg))
		{
			# no special treatment for ops w/o args
		}
		# =literal-constants=
		#
		# arg is an array ref when it contains a w-value. the parser
		# will let a w-value in for ops iff it's a =literal constant=
		#
		elsif (ref($real_arg) eq 'ARRAY')
		{
			my $w_val = calc_w_value($real_arg);
			my $word = value2word($w_val);
			$real_arg = {'A' => [$sym_insert_addr]};
			$mem[$sym_insert_addr++] = $word;
		}
		else
		{
			# handle undefined/future reference symbols in the A-part
			#
			if (scalar(@{$real_arg->{'A'}}) == 1 and is_symbol($real_arg->{'A'}->[0]))
			{
				my $symbol = $real_arg->{'A'}->[0];

				# future references
				#
				if ($symbol =~ /^(\d)F$/)
				{
					my $sym_name = "$1H";
					exists($label_table{$sym_name}) or die "future reference $symbol to a non-existent symbol\n";

					my @defs = @{$label_table{$sym_name}};
					my @def;

					do {@def = pop2(\@defs)}
						while (defined($def[0]) and $def[1] <= $code_line->{line_n});

					defined($def[0]) or die "no $sym_name found after $symbol\n";

					$real_arg->{'A'} = [$def[0]];
				}
				# undefined (non-local) symbols
				#
				elsif ($symbol !~ /^(\d)B$/ and not(exists($label_table{$symbol})))
				{
					my $A_val = $sym_insert_addr;
					$mem[$sym_insert_addr++] = empty_word();
					$real_arg->{'A'} = [$A_val];
				}
			}

			# now translate the local symbols in arg's expressions
			#
			foreach my $expr_ref ($real_arg->{'A'}, $real_arg->{'I'}, $real_arg->{'F'})
			{
				next unless defined($expr_ref);

				for (my $i = 0; $i < @$expr_ref; ++$i)
				{
					if (is_symbol($expr_ref->[$i]) and $expr_ref->[$i] =~ /^(\d)B$/)
					{
						my $sym_name = "$1H";

						my @defs = @{$label_table{$sym_name}};
						my @def;

						do {@def = shift2(\@defs)}
							while (defined($def[0]) and $def[1] >= $code_line->{line_n});

						defined($def[0]) or die "no $sym_name found before $expr_ref->[$i]\n";

						# substitute value
						$expr_ref->[$i] = $def[0];
					}
				}
			}
		}

		#~ print "real arg (line $code_line->{line_n}) of $code_line->{cmd} is: ";
		#~ print Dumper($real_arg), "\n";

		my ($A_val, $I_val) =
			map {
					(exists($real_arg->{$_}) and defined($real_arg->{$_})) ?
						calc_expr_value($real_arg->{$_}) : 0
				}
			('A', 'I');

		my $F_val;

		# for the fspec the default is different, and depends on the command
		#
		if (exists($real_arg->{'F'}) and defined($real_arg->{'F'}))
		{
			$F_val = calc_expr_value($real_arg->{'F'});
		}
		else
		{
			if ($code_line->{cmd} eq "STJ")
			{
				$F_val = 2;
			}
			else
			{
				$F_val = 5;
			}
		}

		my $opcode = $opcode_map{$code_line->{cmd}} or internal_error("bad command $code_line->{cmd}");
		my $C_val = $opcode->[0];
		$F_val = (defined $opcode->[1] ? $opcode->[1] : $F_val);

		my $word = build_instr_word($A_val, $F_val, $I_val, $C_val);
		$mem[$mem_ptr++] = $word;
	};
	if ($@) {assembly_error($@, $code_line)};
}


sub assemble_directive
{
	my ($code_line) = @_;
	my $end_reached = 0;

	eval
	{
		if ($code_line->{cmd} eq "EQU")
		{
			# taken care of during the first pass
		}
		elsif ($code_line->{cmd} eq "ORIG")
		{
			$mem_ptr = calc_w_value($code_line->{arg});
		}
		elsif ($code_line->{cmd} eq "CON")
		{
			my $w_val = calc_w_value($code_line->{arg});
			$mem[$mem_ptr++] = value2word($w_val);
		}
		elsif ($code_line->{cmd} eq "END")
		{
			$first_execution_addr = calc_w_value($code_line->{arg});
			$end_reached = 1;
		}
		elsif ($code_line->{cmd} eq "ALF")
		{
			unless ($code_line->{arg} =~ /\"([^\"]{5})\"/)
			{
				internal_error("illegal ALF argument $code_line\n");
			}

			my @chars = split(//, $1);
			my $word = empty_word();
			my $i = 1;

			foreach my $char (@chars)
			{
				$word->[$i++] = char2byte($char);
			}

			$mem[$mem_ptr++] = $word;
		}
		else
		{
			internal_error("unknown directive $code_line->{cmd}\n");
		}
	};
	if ($@) {assembly_error($@, $code_line)};

	return $end_reached;
}


# Given a code line with a label and a numeric value, checks
# that it's not a label redefinition and sets the label value.
#
sub set_label_value
{
	my ($code_line, $value) = @_;
	my $label_name = $code_line->{label};

	if ($label_name !~ /^\dH$/ and exists($label_table{$label_name}))
	{
		die("Label $label_name redefined\n");
	}

	if (defined($label_table{$label_name}->[0]))
	{
		unshift(@{$label_table{$label_name}}, ($value, $code_line->{line_n}));
	}
	else
	{
		$label_table{$label_name} = [$value, $code_line->{line_n}];
	}
}


sub assembly_error
{
	my ($msg, $code_line) = @_;

	if (defined($code_line) and defined($code_line->{line_n}))
	{
		$msg .= " (at line $code_line->{line_n})";
	}

	die "Assembly error: $msg\n";
}


sub shift2
{
	(@{$_[0]} >= 2) or return undef;
	return splice(@{$_[0]}, 0, 2);
}


sub pop2
{
	(@{$_[0]} >= 2) or return undef;
    return splice(@{$_[0]}, -2);
}


1;
