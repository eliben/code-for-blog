#include <iostream>
#include <string>

int main(int argc, const char** argv) {
  std::string line;
  while (std::getline(std::cin, line)) {
    std::cout << "echo: " << line << "\n";
  }
  return 0;
}
