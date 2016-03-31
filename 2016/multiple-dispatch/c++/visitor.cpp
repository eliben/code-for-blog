// Implementing multiple (double, to be precise) dispatch in C++ with the
// "inverted visitor" pattern, where two virtual dispatches are chained to
// achieve the desired effect.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>
#include <memory>
#include <string>
#include <typeinfo>

class Rectangle;
class Ellipse;

class Shape {
public:
  virtual std::string name() const {
    return typeid(*this).name();
  }

  // Dispatcher that should be called by clients to intersect different shapes.
  virtual void Intersect(const Shape*) const = 0;

  // Specific interesection methods implemented by subclasses. If subclass A
  // has a special way to intersect with subclass B, it should implement
  // InteresectWith(const B*).
  virtual void IntersectWith(const Shape*) const {}
  virtual void IntersectWith(const Rectangle*) const {}
  virtual void IntersectWith(const Ellipse*) const {}
};

class Rectangle : public Shape {
public:
  virtual void Intersect(const Shape* s) const override {
    // Virtual dispatch #2: the static shape of s is Shape*. However, as
    // IntersectWith is a virtual method, the compiler should emit a virtual
    // dispatch that will be routed to the IntersectWith method of the dynamic
    // class of s. The argument passed to IntersectWith has the static type
    // Rectangle*. This is why this method (Intersect) has to be defined in
    // concrete classes like Rectangle and not just inherited from Shape.
    s->IntersectWith(this);
  }

  virtual void IntersectWith(const Shape* s) const override {
    std::cout << "Rectangle x Shape [names this=" << this->name()
              << ", s=" << s->name() << "]\n";
  }

  virtual void IntersectWith(const Rectangle* r) const override {
    std::cout << "Rectangle x Rectangle [names this=" << this->name()
              << ", r=" << r->name() << "]\n";
  }
};

class Ellipse : public Shape {
public:
  virtual void Intersect(const Shape* s) const override {
    s->IntersectWith(this);
  }

  virtual void IntersectWith(const Rectangle* r) const override {
    std::cout << "Ellipse x Rectangle [names this=" << this->name()
              << ", r=" << r->name() << "]\n";
  }
};

class Triangle : public Shape {
public:
  virtual void Intersect(const Shape* s) const override {
    s->IntersectWith(this);
  }
};

int main(int argc, const char** argv) {
  std::unique_ptr<Shape> pr1(new Rectangle);
  std::unique_ptr<Shape> pr2(new Rectangle);
  std::unique_ptr<Shape> pe(new Ellipse);
  std::unique_ptr<Shape> pt(new Triangle);

  std::cout << "Dynamic type dispatch\n";

  // Virtual dispatch #1: statically, pr1 is a Shape*. But Intersect is a
  // virtual method. Therefore pr1->Intersect calls Rectangle::Intersect, since
  // the dynamic type of pr1 is Rectangle. The argument passed to
  // Rectangle::Intersect has the dynamic type Ellipse* but static type Shape*.
  pr1->Intersect(pe.get());
  pr1->Intersect(pr2.get());
  pt->Intersect(pr1.get());

  return 0;
}
