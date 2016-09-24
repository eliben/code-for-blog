// Some ideas of how to implement hierarchical ("sub-command") completion with
// readline, where each sub-command can have its own completion options.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <algorithm>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#include <readline/readline.h>
#include <readline/history.h>

#include "utils.h"

const char* WHITESPACE = " \t";

std::vector<std::string> vocabulary{"file",   "cat", "dog",
                                    "canary", "cow", "hamster"};

bool is_file_command(const std::vector<Token>& tokens, int cursor) {
  if (tokens.size() == 0) {
    return false;
  }
  const Token& command = tokens[0];
  if (static_cast<size_t>(cursor) > command.buf_index + command.text.size() &&
      command.text == "file") {
    return true;
  } else {
    return false;
  }
}

char** completer(const char* text, int start, int end) {
  std::vector<Token> line_tokens = tokenize_line_buffer(rl_line_buffer);
  if (is_file_command(line_tokens, start)) {
    // Request filename completion.
    return nullptr;
  }

  // Disable default filename completion even if we don't find completion
  // matches.
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
