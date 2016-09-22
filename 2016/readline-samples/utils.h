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

#endif /* UTILS_H */
