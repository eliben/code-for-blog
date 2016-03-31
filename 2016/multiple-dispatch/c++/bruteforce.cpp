// "Brute-force" approach based on if-else dynamic checks.
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

void Intersect(const Shape* s1, const Shape* s2) {
  if (const Rectangle* r1 = dynamic_cast<const Rectangle*>(s1)) {
    if (const Rectangle* r2 = dynamic_cast<const Rectangle*>(s2)) {
      std::cout << "Rectangle x Rectangle [names r1=" << r1->name()
                << ", r2=" << r2->name() << "]\n";
    } else if (const Ellipse* e2 = dynamic_cast<const Ellipse*>(s2)) {
      std::cout << "Rectangle x Ellipse [names r1=" << r1->name()
                << ", e2=" << e2->name() << "]\n";

    } else {
      std::cout << "Rectangle x Shape [names r1=" << r1->name()
                << ", s2=" << s2->name() << "]\n";
    }
  } else if (const Ellipse* e1 = dynamic_cast<const Ellipse*>(s1)) {
    if (const Ellipse* e2 = dynamic_cast<const Ellipse*>(s2)) {
      std::cout << "Ellipse x Ellipse [names e1=" << e1->name()
                << ", e2=" << e2->name() << "]\n";
    } else {
      // Handle other Ellipse x ... dispatches.
    }
  }
}

int main(int argc, const char** argv) {
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
