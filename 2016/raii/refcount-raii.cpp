// Synthetic example of refcount that has to be increased or decreased.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>

struct Cursor {
  // dummy
  void incref() {}
  void decref() {}
  bool do_stuff() {return false;}
};

class CursorGuard {
public:
  CursorGuard(Cursor* cursor) : cursor_(cursor) {
    cursor_->incref();
    std::cout << "calling incref\n";
  }

  Cursor* cursor() {
    return cursor_;
  }

  ~CursorGuard() {
    cursor_->decref();
    std::cout << "calling decref\n";
  }

private:
  Cursor* cursor_;
};


void work_with_cursor(Cursor* cursor) {
  CursorGuard cursor_guard(cursor);

  if (cursor_guard.cursor()->do_stuff()) {
    // ... do something
    return;
  }

  // ... do something else
  return;
}


int main(int argc, const char** argv) {
  Cursor c;
  work_with_cursor(&c);
  
  return 0;
}

