// Simple completion with readline.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <string>
#include <vector>

#include <readline/history.h>
#include <readline/readline.h>

std::vector<std::string> vocabulary{"cat", "dog", "canary", "cow", "hamster"};

char* completion_generator(const char* text, int state) {
  // This function is called with state=0 the first time; subsequent calls are
  // with a nonzero state. state=0 can be used to perform one-time
  // initialization for this completion session.
  static std::vector<std::string> matches;
  static size_t match_index = 0;

  if (state == 0) {
    // During initialization, compute the actual matches for 'text' and keep
    // them in a static vector.
    matches.clear();
    match_index = 0;

    // Collect a vector of matches: vocabulary words that begin with text.
    std::string textstr = std::string(text);
    for (auto word : vocabulary) {
      if (word.size() >= textstr.size() &&
          word.compare(0, textstr.size(), textstr) == 0) {
        matches.push_back(word);
      }
    }
  }

  if (match_index >= matches.size()) {
    // We return nullptr to notify the caller no more matches are available.
    return nullptr;
  } else {
    // Return a malloc'd char* for the match. The caller frees it.
    return strdup(matches[match_index++].c_str());
  }
}

char** completer(const char* text, int start, int end) {
  // Don't do filename completion even if our generator finds no matches.
  rl_attempted_completion_over = 1;

  // Note: returning nullptr here will make readline use the default filename
  // completer.
  return rl_completion_matches(text, completion_generator);
}

int main(int argc, char** argv) {
  printf("Welcome! You can exit by pressing Ctrl+C at any time...\n");

  // Register our custom comleter with readline by assigning a function pointer
  // to this global variable.
  rl_attempted_completion_function = completer;

  char* buf;
  while ((buf = readline(">> ")) != nullptr) {
    if (strlen(buf) > 0) {
      add_history(buf);
    }

    printf("[%s]\n", buf);

    // readline malloc'd the buffer; clean it up.
    free(buf);
  }

  return 0;
}
