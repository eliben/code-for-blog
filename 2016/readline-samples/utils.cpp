// Common utils for readline samples.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "utils.h"

#include <algorithm>
#include <cassert>
#include <iostream>

std::string longest_common_prefix(std::string s,
                                  const std::vector<std::string>& candidates) {
  assert(candidates.size() > 0);
  if (candidates.size() == 1) {
    return candidates[0];
  }

  std::string prefix(s);
  while (true) {
    // Each iteration of this loop advances to the next location in all the
    // candidates and sees if they match up to it.
    size_t nextloc = prefix.size();
    auto i = candidates.begin();
    if (i->size() <= nextloc) {
      return prefix;
    }
    char nextchar = (*(i++))[nextloc];
    for (; i != candidates.end(); ++i) {
      if (i->size() <= nextloc || (*i)[nextloc] != nextchar) {
        // Bail out if there's a mismatch for this candidate.
        return prefix;
      }
    }
    // All candidates have contents[nextloc] == nextchar, so we can safely
    // extend the prefix.
    prefix.append(1, nextchar);
  }

  assert(0 && "unreachable");
}

std::vector<Token> tokenize_line_buffer(const std::string& buf) {
  const char* delims = " \t";
  std::vector<Token> tokens;

  // Skip leading delimiters to the first token.
  size_t istart = buf.find_first_not_of(delims);
  while (istart != std::string::npos) {
    // Invariant: istart points at the beginning of a token inside buf.
    size_t iend = buf.find_first_of(delims, istart);
    if (iend == std::string::npos) {
      iend = buf.size();
    }

    tokens.push_back({buf.substr(istart, iend - istart), istart});
    istart = buf.find_first_not_of(delims, iend);
  }

  return tokens;
}

void show_tokens(const std::vector<Token>& tv) {
  std::cout << "[" << tv.size() << " tokens]: ";
  for (const auto& t : tv) {
    std::cout << t.text << "[" << t.buf_index << "] ";
  }
  std::cout << "\n";
}
