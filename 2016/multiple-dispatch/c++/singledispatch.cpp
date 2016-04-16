// Simple single dispatch - virtual method call.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>
#include <memory>
#include <string>

class Shape {
public:
  virtual void ComputeArea() const = 0;
};

class Rectangle : public Shape {
public:
  virtual void ComputeArea() const {
    std::cout << "Rectangle: width times height\n";
  }
};

class Ellipse : public Shape {
public:
  virtual void ComputeArea() const {
    std::cout << "Ellipse: width times height times pi/4\n";
  }
};


int main(int argc, const char** argv) {
  std::unique_ptr<Shape> pr(new Rectangle);
  std::unique_ptr<Shape> pe(new Ellipse);

  pr->ComputeArea();
  pe->ComputeArea();
  
  return 0;
}

