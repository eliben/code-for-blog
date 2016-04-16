// Demonstrates a naive attempt at multiple dispatch that doesn't really work
// at runtime, only at compile time.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>
#include <memory>
#include <string>
#include <typeinfo>

class Shape {
public:
  virtual std::string name() const {
    return typeid(*this).name();
  }
};

class Rectangle : public Shape {};

class Ellipse : public Shape {};

class Triangle : public Shape {};

// Overloaded Intersect methods.
void Intersect(const Rectangle* r, const Ellipse* e) {
  std::cout << "Rectangle x Ellipse [names r=" << r->name()
            << ", e=" << e->name() << "]\n";
}

void Intersect(const Rectangle* r1, const Rectangle* r2) {
  std::cout << "Rectangle x Rectangle [names r1=" << r1->name()
            << ", r2=" << r2->name() << "]\n";
}

// ...

void Intersect(const Shape* s1, const Shape* s2) {
  std::cout << "Shape x Shape [names s1=" << s1->name() << ", s2=" << s2->name()
            << "]\n";
}

int main(int argc, const char** argv) {
  Rectangle r1, r2;
  Ellipse e;
  Triangle t;

  std::cout << "Static type dispatch\n";
  Intersect(&r1, &e);
  Intersect(&r1, &r2);
  Intersect(&r1, &t);

  std::unique_ptr<Shape> pr1(new Rectangle);
  std::unique_ptr<Shape> pr2(new Rectangle);
  std::unique_ptr<Shape> pe(new Ellipse);
  std::unique_ptr<Shape> pt(new Triangle);

  std::cout << "Dynamic type dispatch\n";
  Intersect(pr1.get(), pe.get());
  Intersect(pr1.get(), pr2.get());
  Intersect(pr1.get(), pt.get());
  
  return 0;
}
