// Common utils for readline samples.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "utils.h"

#include <algorithm>
#include <cassert>

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
