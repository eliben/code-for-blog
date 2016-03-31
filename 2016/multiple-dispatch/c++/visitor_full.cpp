// A more complete sample of the visitor pattern for multiple dispatch.
// Separates header and implementation so that the implementations of classes
// can circularly refer to other classes.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "visitor_full.h"

#include <iostream>
#include <memory>

namespace {

// All intersections between rectangles and ellipses dispatch here: symmatrical!
void IntersectRectangleEllipse(const Rectangle* r, const Ellipse* e) {
  std::cout << "IntersectRectangleEllipse [names r=" << r->name()
            << ", e=" << e->name() << "]\n";
}

}

void Shape::IntersectWith(const Shape* s) const {
  std::cout << "Shape x Shape [names this=" << this->name()
            << ", s=" << s->name() << "]\n";
}

void Shape::IntersectWith(const Rectangle* r) const {
  std::cout << "Shape x Rectangle [names this=" << this->name()
            << ", r=" << r->name() << "]\n";
}

void Shape::IntersectWith(const Ellipse* e) const {
  std::cout << "Shape x Ellipse [names this=" << this->name()
            << ", e=" << e->name() << "]\n";
}

void Shape::IntersectWith(const Triangle* t) const {
  std::cout << "Shape x Triangle [names this=" << this->name()
            << ", t=" << t->name() << "]\n";
}

void Rectangle::IntersectWith(const Shape* s) const {
  // For symmetry call Shape::IntersectWith(Rectangle)
  s->IntersectWith(this);
}

void Rectangle::IntersectWith(const Rectangle* r) const {
  std::cout << "Rectangle x Rectangle [names this=" << this->name()
            << ", r=" << r->name() << "]\n";
}

void Rectangle::IntersectWith(const Ellipse* e) const {
  IntersectRectangleEllipse(this, e);
}

void Ellipse::IntersectWith(const Shape* s) const {
  // For symmetry call Shape::IntersectWith(Rectangle)
  s->IntersectWith(this);
}

void Ellipse::IntersectWith(const Rectangle* r) const {
  IntersectRectangleEllipse(r, this);
}

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
