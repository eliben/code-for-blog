// Basic terminal echoing without readline.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>
#include <string>

int main(int argc, char** argv) {
  printf("Welcome! You can exit by pressing Ctrl+C at any time...\n");

  std::string line;
  std::cout << ">> ";
  while (std::getline(std::cin, line)) {
    std::cout << "[" << line << "]\n";
    std::cout << ">> ";
  }

  return 0;
}
