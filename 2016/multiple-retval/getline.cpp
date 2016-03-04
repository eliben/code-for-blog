// Example of getline for return value in a parameter.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>
#include <string>

int main(int argc, const char** argv) {
  std::string line;
  while (std::getline(std::cin, line)) {
    std::cout << "echo: " << line << "\n";
  }
  return 0;
}
