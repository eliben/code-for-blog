// Like readline-complete-simple, but without using rl_completion_matches
// and a stateful generator function. Instead, return the sequence of matches
// directly from our completion function.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <algorithm>
#include <cassert>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#include <readline/readline.h>
#include <readline/history.h>

std::vector<std::string> vocabulary{"cat", "dog", "canary", "cow", "hamster"};

// Find the longest common prefix among all candidates; the prefix begins with
// `s`. `s` may be an empty string, but if it's not empty then all candidates
// begin with it. The candidates vector must be non-empty.
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

void test_longest_common_prefix() {
  assert(longest_common_prefix("foo", {"foob", "foobar"}) == "foob");
  assert(longest_common_prefix("foo", {"foob"}) == "foob");
  assert(longest_common_prefix("foo", {"foo", "foobar"}) == "foo");
  assert(longest_common_prefix("k", {"kbc1", "kbc2", "kbc2"}) == "kbc");
  assert(longest_common_prefix("k", {"kbc1"}) == "kbc1");
  assert(longest_common_prefix("k", {"kbc1", "kbb", "kbc2"}) == "kb");
  assert(longest_common_prefix("", {"kbc1", "kbb", "kbc2"}) == "kb");
  assert(longest_common_prefix("k", {"kba1", "kba2", "kba3"}) == "kba");
}

// This completer returns the char** array required by readline without invoking
// the state-ful generator via rl_completion_matches.
// The structure of the returned array is as follows (taken from the source of
// readline):
//
//    The first entry in the returned array is the substitution for TEXT.
//    The remaining entries are the possible completions.
//    The array is terminated with a NULL pointer.
//
// The returned char** array is malloc'd herein; the caller frees it. If the
// returned array is nullptr, it means there were no matches.
char** completer(const char* text, int start, int end) {
  // Don't do filename completion even if our generator finds no matches.
  rl_attempted_completion_over = 1;

  // Filter out all words in the vocabulary that do not begin with `text`.
  std::string textstr(text);
  std::vector<std::string> matches;
  std::copy_if(vocabulary.begin(), vocabulary.end(),
               std::back_inserter(matches), [&textstr](const std::string& s) {
                 return (s.size() >= textstr.size() &&
                         s.compare(0, textstr.size(), textstr) == 0);
               });
  if (matches.empty()) {
    return nullptr;
  }

  char** array =
      static_cast<char**>(malloc((2 + matches.size()) * sizeof(*array)));
  array[0] = strdup(longest_common_prefix(textstr, matches).c_str());
  size_t ptr = 1;
  for (const auto& m : matches) {
    array[ptr++] = strdup(m.c_str());
  }
  array[ptr] = nullptr;
  return array;
}

int main(int argc, char** argv) {
  printf("Welcome! You can exit by pressing Ctrl+C at any time...\n");

  test_longest_common_prefix();

  // Register our custom comleter with readline.
  rl_attempted_completion_function = completer;

  char* buf;
  while ((buf = readline(">> ")) != nullptr) {
    if (strlen(buf) > 0) {
      add_history(buf);
    }

    printf("[%s]\n", buf);

    // readline malloc's a new buffer every time.
    free(buf);
  }

  return 0;
}
