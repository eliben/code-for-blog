// For the blog post "Classical inheritance in JS ES5"
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.

// Shape - superclass
// x,y: location of shape's bounding rectangle
function Shape(x, y) {
  this.x = x;
  this.y = y;
}

// Superclass method
Shape.prototype.move = function(x, y) {
  this.x += x;
  this.y += y;
}

// Circle - subclass
function Circle(x, y, r) {
  // Call constructor of superclass to initialize superclass-derived members.
  Shape.call(this, x, y);

  // Initialize subclass's own members
  this.r = r;
}

// Circle derives from Shape
Circle.prototype = Object.create(Shape.prototype);
Circle.prototype.constructor = Circle;

// Subclass methods. Add them after Circle.prototype is created with
// Object.create
Circle.prototype.circumference = function() {
  return this.r * 2 * Math.PI;
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
