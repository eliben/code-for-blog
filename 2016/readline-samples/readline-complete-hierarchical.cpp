#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#include <readline/readline.h>
#include <readline/history.h>

const char* WHITESPACE = " \t";

std::vector<std::string> vocabulary{"file",   "cat", "dog",
                                    "canary", "cow", "hamster"};

// Represents tokens in the line buffer. For each token, we have its text, as
// well as its index in the buffer (the offset of its first character from the
// buffer's beginning).
struct Token {
  std::string text;
  size_t buf_index;
};

// Split the given buffer to a vector of Tokens.
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

char* completion_generator(const char* text, int state) {
  // This function is called with state=0 the first time; subsequent calls are
  // with a nonzero state. state=0 can be used to perform one-time
  // initialization for this completion session.
  static std::vector<std::string> matches;
  static size_t match_index = 0;

  if (state == 0) {
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
  std::vector<Token> line_tokens = tokenize_line_buffer(rl_line_buffer);
  if (line_tokens.size() > 0) {
    const Token& command = line_tokens[0];
    if (static_cast<size_t>(start) > command.buf_index + command.text.size() &&
        command.text == "file") {
      // If the "file" command (first token) was already entered and we're now
      // trying to complete the next token, return nullptr so that the default
      // file completer will be activated.
      return nullptr;
    }
  }

  // Use custom completion; disable default filename completion even if we don't
  // find completion matches.
  ::rl_attempted_completion_over = 1;
  return ::rl_completion_matches(text, completion_generator);
}

int main(int argc, char** argv) {
  printf("Welcome! You can exit by pressing Ctrl+C at any time...\n");

  show_tokens(tokenize_line_buffer("hola "));
  show_tokens(tokenize_line_buffer(" cow foo hola"));

  // Register our custom comleter with readline.
  ::rl_attempted_completion_function = completer;

  char* buf;
  while ((buf = ::readline(">> ")) != nullptr) {
    if (strlen(buf) > 0) {
      ::add_history(buf);
    }

    printf("[%s]\n", buf);

    // ::readline malloc's a new buffer every time.
    free(buf);
  }

  return 0;
}
