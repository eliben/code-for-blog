// ES6 variation with classes for "Classical inheritance in JS ES5".
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.

class Shape {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  move(x, y) {
    this.x += x;
    this.y += y;
  }
}

class Circle extends Shape {
  constructor(x, y, r) {
    super(x, y);
    this.r = r;
  }

  circumference() {
    return this.r * 2 * Math.PI;
  }
}

var shp = new Shape(1, 2);
console.log([shp.x, shp.y]);
shp.move(1, 1);
console.log([shp.x, shp.y]);

var cir = new Circle(5, 6, 2);
console.log([cir.x, cir.y, cir.r]);
cir.move(1, 1);
console.log([cir.x, cir.y, cir.r]);
console.log(cir.circumference());

console.log("instanceof tests:");
console.log(cir instanceof Shape);
console.log(cir instanceof Circle);
console.log(shp instanceof Shape);
console.log(shp instanceof Circle);
