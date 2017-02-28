// Parser for BF programs.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#ifndef PARSER_H
#define PARSER_H

#include <iostream>
#include <string>

struct Program {
  std::string instructions;
};

// Parses a BF program from an input stream. Returns a Program if successful; on
// error, dies with an error message.
Program parse_from_stream(std::istream& stream);

#endif /* PARSER_H */
