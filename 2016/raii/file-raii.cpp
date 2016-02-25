// Simple example of RAII for opening and closing a FILE* handle
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <cstdio>
#include <iostream>
#include <string>

class FileHandle {
  public:
    FileHandle(const char* name, const char* mode) {
      f_ = fopen(name, mode);
      std::cout << "FileHandle opened handle " << f_ << "\n";
    }

    FILE* file() {
      return f_;
    }

    ~FileHandle() {
      if (f_ != nullptr) {
        fclose(f_);
        std::cout << "~FileHandle closed handle " << f_  << "\n";
      }
    }

  private:
    FILE* f_;
};

std::string do_stuff_with_file(std::string filename) {
  FileHandle handle(filename.c_str(), "r");
  int firstchar = fgetc(handle.file());

  if (firstchar != '$') {
    return "bad bad bad";
  }

  return std::string(1, firstchar);
}

int main(int argc, const char** argv) {
  std::string result = do_stuff_with_file("Makefile");
  std::cout << "result = " << result << "\n";
  
  return 0;
}
