// A more complete sample of the visitor pattern for multiple dispatch.
// Separates header and implementation so that the implementations of classes
// can circularly refer to other classes.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "visitor_full.h"

#include <iostream>
#include <memory>

void Shape::IntersectWith(const Shape* s) const {
  std::cout << "Shape x Shape [names this=" << this->name()
            << ", s=" << s->name() << "]\n";
}

// All specific shape intersects are dispatching to the Shape x Shape
// intersection. They have to be implemented for the compiling & linking to
// succeed, but they can be trivially inlined by the compiler.
void Shape::IntersectWith(const Rectangle* r) const {
  IntersectWith(static_cast<const Shape*>(r));
}

void Shape::IntersectWith(const Ellipse* e) const {
  IntersectWith(static_cast<const Shape*>(e));
}

void Shape::IntersectWith(const Triangle* t) const {
  IntersectWith(static_cast<const Shape*>(t));
}

void Rectangle::IntersectWith(const Rectangle* r) const {
  std::cout << "Rectangle x Rectangle [names this=" << this->name()
            << ", r=" << r->name() << "]\n";
}

namespace {

// All intersections between rectangles and ellipses dispatch here to show how
// symmetry may work.
void SymmetricIntersectRectangleEllipse(const Rectangle* r, const Ellipse* e) {
  std::cout << "IntersectRectangleEllipse [names r=" << r->name()
            << ", e=" << e->name() << "]\n";
}
}

void Rectangle::IntersectWith(const Ellipse* e) const {
  SymmetricIntersectRectangleEllipse(this, e);
}

void Ellipse::IntersectWith(const Rectangle* r) const {
  SymmetricIntersectRectangleEllipse(r, this);
}

// Emit the statement's text before calling it.
#define LOG(x)                                                                 \
  do {                                                                         \
    std::cout << #x << ":       ";                                             \
    (x);                                                                       \
  } while (0);

int main(int argc, const char** argv) {
  std::unique_ptr<Shape> pr1(new Rectangle);
  std::unique_ptr<Shape> pr2(new Rectangle);
  std::unique_ptr<Shape> pe1(new Ellipse);
  std::unique_ptr<Shape> pe2(new Ellipse);
  std::unique_ptr<Shape> pt(new Triangle);

  std::cout << "Dynamic type dispatch\n";

  LOG(pr1->Intersect(pe1.get()));
  LOG(pe1->Intersect(pr1.get()));
  LOG(pr1->Intersect(pr2.get()));

  // Here the dispatch is routed to Shape, since Ellipse doesn't define
  // intersections with ellipses.
  LOG(pe1->Intersect(pe2.get()));

  // Since Rectangle and Triangle don't define intersection ops with each other,
  // these calls are routed to Shape.
  LOG(pr1->Intersect(pt.get()));
  LOG(pt->Intersect(pr1.get()));

  return 0;
}
