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
#include <unordered_map>
#include <vector>

#include <readline/readline.h>
#include <readline/history.h>

#include "utils.h"

// Global logfile for debugging. Using a separate file so that output doesn't
// intermix with the actual readline terminal. That file can be watched with
// "tail -f" in a separate window.
FILE* logfile;

// Mapping commands to possible subcommands. Populate this map with
// register_command.
using CommandMap = std::unordered_map<std::string, std::vector<std::string>>;
CommandMap command_map;

void register_command(const std::string& command, const
                      std::vector<std::string>& subcommands) {
  command_map[command] = subcommands;
}

char** completer(const char* text, int start, int end) {
  fprintf(logfile, "[%s (%d:%d)]\n", text, start, end);
  // The vocabulary for this completion session. Will be populated by one of the
  // conditions below based on parsing the text so far.
  std::vector<std::string> vocabulary;

  std::vector<Token> line_tokens = tokenize_line_buffer(rl_line_buffer);
  size_t cursor = start;
  if (line_tokens.size() == 0 ||
      (line_tokens.size() == 1 && cursor == line_tokens[0].buf_index)) {
    // If either:
    //
    // - There are no tokens yet, or
    // - There's one token already and start points at its beginning of
    //
    // This means we're trying to complete the first token -- a command.
    for (auto& kv : command_map) {
      vocabulary.push_back(kv.first);
    }
  } else if ((line_tokens.size() == 1 &&
              cursor > line_tokens[0].buf_index + line_tokens[0].text.size()) ||
             (line_tokens.size() == 2 && cursor == line_tokens[1].buf_index)) {
    // If either:
    //
    // - There is a single token already and starts points past its end, or
    // - There are two tokens already and start points at the beginning of the
    //   second
    //
    // This means we're trying to complete the second token -- a subcommand.

    const std::string command = line_tokens[0].text;
    if (command == "file") {
      // File is special. We attempt filename completion.
      rl_attempted_completion_over = 0;
      return nullptr;
    }

    const auto it = command_map.find(command);
    if (it != command_map.end()) {
      vocabulary = it->second;
    }
  } else {
    // Otherwise don't complete at all.
    rl_attempted_completion_over = 1;
    return nullptr;
  }

  // Filter out all words in the vocabulary that do not begin with `text`.
  std::string textstr(text);
  std::vector<std::string> matches;
  std::copy_if(vocabulary.begin(), vocabulary.end(),
               std::back_inserter(matches), [&textstr](const std::string& s) {
                 return (s.size() >= textstr.size() &&
                         s.compare(0, textstr.size(), textstr) == 0);
               });
  fprintf(logfile, "Found matches: [");
  for (const auto& m : matches) {
    fprintf(logfile, "%s ", m.c_str());
  }
  fprintf(logfile, "]\n");

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
  logfile = fopen("/tmp/rllog", "w");
  if (!logfile) {
    std::cout << "Unable to open log file for writing!\n";
    return 1;
  }
  // Disable buffering so that logging appears immediately.
  setbuf(logfile, NULL);

  printf("Welcome! You can exit by pressing Ctrl+C at any time...\n");
  register_command("season", {"winter", "spring", "summer", "fall"});
  register_command("animal", {"cat", "dog", "canary", "cow", "hamster"});
  register_command("file", {});

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
