// Common utils for readline samples.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#ifndef UTILS_H
#define UTILS_H

#include <string>
#include <vector>

// Find the longest common prefix among all candidates; the prefix begins with
// `s`. `s` may be an empty string, but if it's not empty then all candidates
// begin with it. The candidates vector must be non-empty.
std::string longest_common_prefix(std::string s,
                                  const std::vector<std::string>& candidates);

// Represents tokens in the line buffer. For each token, we have its text, as
// well as its index in the buffer (the offset of its first character from the
// buffer's beginning).
struct Token {
  std::string text;
  size_t buf_index;
};

// Split the given buffer to a vector of Tokens. Delimeters are space and tab.
std::vector<Token> tokenize_line_buffer(const std::string& buf);

// Show a vector of tokens by printing it to stdout.
void show_tokens(const std::vector<Token>& tv);

#endif /* UTILS_H */
