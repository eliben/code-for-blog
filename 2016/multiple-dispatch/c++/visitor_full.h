// A more complete sample of the visitor pattern for multiple dispatch.
// Separates header and implementation so that the implementations of classes
// can circularly refer to other classes.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#ifndef VISITOR_FULL_H
#define VISITOR_FULL_H

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
    s->IntersectWith(this);
  }

  virtual void IntersectWith(const Shape* s) const override;
  virtual void IntersectWith(const Rectangle*) const override;
  virtual void IntersectWith(const Ellipse*) const override;
};

class Ellipse : public Shape {
public:
  virtual void Intersect(const Shape* s) const override {
    s->IntersectWith(this);
  }

  virtual void IntersectWith(const Shape* s) const override;
  virtual void IntersectWith(const Rectangle*) const override;
  virtual void IntersectWith(const Ellipse*) const override;
};

#endif // VISITOR_FULL_H
