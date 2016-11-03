// Some ideas of how to implement hierarchical ("sub-command") completion with
// readline, where each sub-command can have its own completion options.
//
// This is a very simple implementation just to demonstrate a point. In a
// realistic program things would be better factored out. Note that using
// globals at least to some degree is unavoidable since readline takes bare
// functions pointers into C code, so there's no way to pass in state/context.
// Luckily, command-line completion is handled in a singple place in an
// application anyway.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>
#include <vector>

#include <readline/history.h>
#include <readline/readline.h>

#include "utils.h"

// Maps a command to a vector of subcommands it supports.
using CommandVocabulary = std::map<std::string, std::vector<std::string>>;

CommandVocabulary command_vocabulary = {
    // "file" is a special command with no subcommands.
    {"file", {}},
    {"eat", {"breakfast", "dinner", "lunch", "snack"}},
    {"play", {"cards", "chess", "go"}},
    {"walk", {"left", "right", "straight"}}};

// All supported commands - populated from command_vocabulary in main().
std::vector<std::string> all_commands;

// Determines if we're supposed to complete a sub-command now. Takes the input
// line typed in so far, split to tokens, and the cursor location. Returns the
// command name if a subcommand is expected (the command is always the first
// token on the line), and "" if it seems like we're still completing the
// command itself.
std::string find_command(const std::vector<Token>& tokens, int cursor) {
  if (tokens.size() == 0) {
    return "";
  }
  const Token& command = tokens[0];
  if (static_cast<size_t>(cursor) > command.buf_index + command.text.size()) {
    return command.text;
  } else {
    return "";
  }
}

char** completer(const char* text, int start, int end) {
  std::vector<Token> line_tokens = tokenize_line_buffer(rl_line_buffer);
  std::string command = find_command(line_tokens, start);
  // The "file" command is special and must be intercepted early, so that we
  // can return nullptr from here before setting rl_attempted_completion_over.
  if (command == "file") {
    // Request filename completion.
    return nullptr;
  }

  // Disable default filename completion even if we don't find completion
  // matches.
  rl_attempted_completion_over = 1;

  // Find which vocabulary to auto-complete from; if we're now completing the
  // command, point to all_commands. If the command was already entered, find
  // the subcommands vocabulary to complete.
  // Note: this will perform subcommand completion for every token after the
  // first one (which is the command). This can be easily avoided if necessary,
  // by checking how many tokens the line already has.
  std::vector<std::string>* vocabulary;
  if (command == "") {
    vocabulary = &all_commands;
  } else {
    auto command_iter = command_vocabulary.find(command);
    if (command_iter == command_vocabulary.end()) {
      return nullptr;
    }
    vocabulary = &(command_iter->second);
  }

  // Filter out all words in the vocabulary that do not begin with `text`.
  std::string textstr(text);
  std::vector<std::string> matches;
  std::copy_if(vocabulary->begin(), vocabulary->end(),
               std::back_inserter(matches), [&textstr](const std::string& s) {
                 return (s.size() >= textstr.size() &&
                         s.compare(0, textstr.size(), textstr) == 0);
               });

  if (matches.empty()) {
    return nullptr;
  }

  // See the readline-complete-nogen.cpp sample for more details on what is
  // returned from this function.
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

  for (auto const& kv : command_vocabulary) {
    all_commands.push_back(kv.first);
  }

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
