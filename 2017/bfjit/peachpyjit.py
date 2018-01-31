# BF JIT using the PeachPy x86-64 codegen library.
#
# Tested with Python 3.5+; PeachPy installed into a virtualenv following the
# installation steps in its README: https://github.com/Maratyszcza
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import argparse
from collections import namedtuple
import ctypes
import sys

import peachpy
import peachpy.x86_64


def die(msg):
    print('ERROR:', msg)
    sys.exit(1)


def parse_bf_program(file):
    """Parses BF programs from the given file-like object.

    Yields BF commands found in the file.
    """
    for line in file:
        for c in line:
            if c in {'>', '<', '+', '-', '.', ',', '[', ']'}:
                yield c


# Data type for the state of PeachPy labels created for every matching bracket
# pair in BF ("[" ... "]").
BracketLabels = namedtuple('BracketLabels', ('open_label', 'close_label'))


def peachpyjit(bf_file, verbose=False):
    """JIT-compiles and executes the given BF program.

    bf_file is a file-like object containing a sequence of BF instructions. The
    result of this function is the possible side-effects of the BF program being
    executed (user input through stdin, output through stdout, etc.)
    """
    open_bracket_stack = []

    # Create a JITed function named "ppjit", with the C-style signature:
    #   void ppjit(uint8_t* memptr)
    #
    memptr = peachpy.Argument(peachpy.ptr(peachpy.uint8_t))

    with peachpy.x86_64.Function("ppjit",
                                 [memptr],
                                 result_type=None) as asm_function:
        # Use r13 as our data pointer; initially it points at the memory buffer
        # passed into the JITed function.
        dataptr = peachpy.x86_64.r13
        peachpy.x86_64.LOAD.ARGUMENT(dataptr, memptr)

        for pc, instr in enumerate(parse_bf_program(bf_file), start=1):
            if instr == '>':
                peachpy.x86_64.ADD(dataptr, 1)
            elif instr == '<':
                peachpy.x86_64.SUB(dataptr, 1)
            elif instr == '+':
                peachpy.x86_64.ADD([dataptr], 1)
            elif instr == '-':
                peachpy.x86_64.SUB([dataptr], 1)
            elif instr == '.':
                # Invoke the WRITE syscall (rax=1) with stdout (rdi=1).
                if sys.platform == "darwin":
                    peachpy.x86_64.MOV(peachpy.x86_64.rax, 0x2000004)
                else:
                    peachpy.x86_64.MOV(peachpy.x86_64.rax, 1)
                peachpy.x86_64.MOV(peachpy.x86_64.rdi, 1)
                peachpy.x86_64.MOV(peachpy.x86_64.rsi, dataptr)
                peachpy.x86_64.MOV(peachpy.x86_64.rdx, 1)
                peachpy.x86_64.SYSCALL()
            elif instr == ',':
                # Invoke the READ syscall (rax=0) with stdin (rdi=0).
                if sys.platform == "darwin":
                    peachpy.x86_64.MOV(peachpy.x86_64.rax, 0x2000003)
                else:
                    peachpy.x86_64.MOV(peachpy.x86_64.rax, 0)
                peachpy.x86_64.MOV(peachpy.x86_64.rdi, 0)
                peachpy.x86_64.MOV(peachpy.x86_64.rsi, dataptr)
                peachpy.x86_64.MOV(peachpy.x86_64.rdx, 1)
                peachpy.x86_64.SYSCALL()
            elif instr == '[':
                # Create labels for the loop start and after-loop.
                loop_start_label = peachpy.x86_64.Label()
                loop_end_label = peachpy.x86_64.Label()
                # Jump to after the loop if the current cell is 0.
                peachpy.x86_64.CMP([dataptr], 0)
                peachpy.x86_64.JZ(loop_end_label)
                # Bind the "start loop" label here.
                peachpy.x86_64.LABEL(loop_start_label)
                open_bracket_stack.append(
                    BracketLabels(loop_start_label, loop_end_label))
            elif instr == ']':
                if not len(open_bracket_stack):
                    die('unmatched closing "]" at pc={}'.format(pc))
                labels = open_bracket_stack.pop()
                # Jump back to loop if the current cell is not 0.
                peachpy.x86_64.CMP([dataptr], 0)
                peachpy.x86_64.JNZ(labels.open_label)
                # Bind the "after-loop" label here.
                peachpy.x86_64.LABEL(labels.close_label)

        peachpy.x86_64.RETURN()

    # Finalize and encode the PeachPy function; python_function will be a
    # callable representing the JITed function in memory.
    abi = peachpy.x86_64.abi.detect()
    encoded_function = asm_function.finalize(abi).encode()
    python_function = encoded_function.load()

    if verbose:
        code = python_function.code_segment
        fname = '/tmp/ppout.bin'
        with open(fname, 'wb') as f:
            f.write(code)
        print('* Wrote machine code to {}'.format(fname))

    # Allocate memory as a ctypes array and initialize it to 0s. Then perform
    # the JIT call.
    memsize = 30000
    MemoryArrayType = ctypes.c_uint8 * memsize
    memory = MemoryArrayType(*([0] * memsize))

    python_function(memory)

    if verbose:
        print('* Memory nonzero locations:')
        pcount = 0
        for i in range(memsize):
            if memory[i]:
                print('[{:3}] = {:3}'.format(i, memory[i]), end=' ')
                pcount += 1
                if pcount > 0 and pcount % 4 == 0:
                    print('')
        print('')


if __name__ == '__main__':
    argparser = argparse.ArgumentParser()
    argparser.add_argument('bffile', type=str)
    argparser.add_argument('--verbose', action='store_true')
    args = argparser.parse_args()

    # Bump Python call stack size to work around
    # https://github.com/Maratyszcza/PeachPy/issues/74
    # PeachPy's "finalization" of large functions may still take quite a bit of
    # time though.
    sys.setrecursionlimit(10000)

    with open(args.bffile) as f:
        peachpyjit(f, verbose=args.verbose)
