// A more complete sample of the visitor pattern for multiple dispatch.
// Separates header and implementation so that the implementations of classes
// can circularly refer to other classes.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "visitor_full.h"

#include <iostream>
#include <memory>

void Rectangle::IntersectWith(const Shape* s) const {
  std::cout << "Rectangle x Shape [names this=" << this->name()
            << ", s=" << s->name() << "]\n";
}

void Rectangle::IntersectWith(const Rectangle* r) const {
  std::cout << "Rectangle x Rectangle [names this=" << this->name()
            << ", r=" << r->name() << "]\n";
}

void Rectangle::IntersectWith(const Ellipse* e) const {
}

void Ellipse::IntersectWith(const Shape* s) const {
}

void Ellipse::IntersectWith(const Rectangle* r) const {
  std::cout << "Ellipse x Rectangle [names this=" << this->name()
            << ", r=" << r->name() << "]\n";
}

void Ellipse::IntersectWith(const Ellipse* e) const {
}

int main(int argc, const char** argv) {
  std::unique_ptr<Shape> pr1(new Rectangle);
  std::unique_ptr<Shape> pr2(new Rectangle);
  std::unique_ptr<Shape> pe(new Ellipse);

  std::cout << "Dynamic type dispatch\n";

  // Virtual dispatch #1: statically, pr1 is a Shape*. But Intersect is a
  // virtual method. Therefore pr1->Intersect calls Rectangle::Intersect, since
  // the dynamic type of pr1 is Rectangle. The argument passed to
  // Rectangle::Intersect has the dynamic type Ellipse* but static type Shape*.
  pr1->Intersect(pe.get());
  pr1->Intersect(pr2.get());

  return 0;
}
