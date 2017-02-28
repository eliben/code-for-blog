// Parser for BF programs.
//
// Parsing BF is trivial: the language's control characters in the stream
// are collected together; all other characters are ignored.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "parser.h"
#include "utils.h"

#include <iostream>
#include <sstream>

Program parse_from_stream(std::istream& stream) {
  Program program;

  for (std::string line; std::getline(stream, line);) {
    for (auto c : line) {
      if (c == '>' || c == '<' || c == '+' || c == '-' || c == '.' ||
          c == ',' || c == '[' || c == ']') {
        program.instructions.push_back(c);
      }
    }
  }
  return program;
}
