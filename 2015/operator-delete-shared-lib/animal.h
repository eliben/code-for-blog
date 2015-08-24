class Animal {
public:
  virtual void say() = 0;
  virtual ~Animal() {}
};

typedef Animal* (*animal_factory)();
