# BF JIT using the PeachPy x68-64 codegen library.
#
# Tested with Python 3.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
from collections import namedtuple
import ctypes
import logging
import sys

import peachpy
import peachpy.x86_64


def die(msg):
  logging.error(msg)
  sys.exit(1)


def parse_bf_program(file):
  for line in file:
    for c in line:
      if c in {'>', '<', '+', '-', '.', ',', '[', ']'}:
        yield c


BracketLabels = namedtuple('BracketLabels', ('open_label', 'close_label'))


def peachpyjit(bf_file):
  """JIT-compiles and executes the given BF program.

  bf_file is a file-like object containing a sequence of BF instructions. The
  result of this function is the possible side-effects of the BF program being
  executed (user input through stdin, output through stdout, etc.)
  """
  open_bracket_stack = []

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
        # Invoke the WRITE syscall to stdout.
        peachpy.x86_64.MOV(peachpy.x86_64.rax, 1)
        peachpy.x86_64.MOV(peachpy.x86_64.rdi, 1)
        peachpy.x86_64.MOV(peachpy.x86_64.rsi, dataptr)
        peachpy.x86_64.MOV(peachpy.x86_64.rdx, 1)
        peachpy.x86_64.SYSCALL()
      elif instr == ',':
        # Invoke the READ syscall to stdin.
        peachpy.x86_64.MOV(peachpy.x86_64.rax, 0)
        peachpy.x86_64.MOV(peachpy.x86_64.rdi, 0)
        peachpy.x86_64.MOV(peachpy.x86_64.rsi, dataptr)
        peachpy.x86_64.MOV(peachpy.x86_64.rdx, 1)
        peachpy.x86_64.SYSCALL()
      elif instr == '[':
        # Create labels for the loop start and after-loop.
        loop_start_label = peachpy.x86_64.Label('loop_start{}'.format(pc))
        loop_end_label = peachpy.x86_64.Label('after_loop{}'.format(pc))

        # Jump to after the loop if the current cell is 0.
        peachpy.x86_64.CMP([dataptr], 0)
        peachpy.x86_64.JZ(loop_end_label)
        # Bind the "start loop" label here.
        peachpy.x86_64.LABEL(loop_start_label)
        open_bracket_stack.append(BracketLabels(loop_start_label,
                                                loop_end_label))
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

  abi = peachpy.x86_64.abi.detect()
  encoded_function = asm_function.finalize(abi).encode()
  python_function = encoded_function.load()
  code = python_function.code_segment

  # TODO: do this in verbose mode
  fname = '/tmp/ppout.bin'
  with open(fname, 'wb') as f:
      f.write(code)

  # The JIT call. Allocate memory as a ctypes array.
  memsize = 30000
  MemoryArrayType = ctypes.c_uint8 * memsize
  memory = MemoryArrayType(*([0] * memsize))

  python_function(memory)

  # TODO: do this in verbose mode and wrap appropriately.
  #for i in range(memsize):
    #if memory[i]:
      #print('[{}] = {}'.format(i, memory[i]))


if __name__ == '__main__':
  # Bump Python call stack size to work around
  # https://github.com/Maratyszcza/PeachPy/issues/74
  # With this in place, PeachPy takes a bit of time to finalize the function but
  # at least it then runs...
  sys.setrecursionlimit(10000)

  if len(sys.argv) < 2:
    die('expecting <bf program name>')

  with open(sys.argv[1]) as f:
    peachpyjit(f)
