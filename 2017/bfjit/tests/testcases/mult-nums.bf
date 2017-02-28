[
  Multiplies x by y, leaving the result in x; then prints x.

  c0: x
  c1: y
  c2: temp0
  c3: temp1
]

      Initialize x and y
++++
> +++++

      Copy x to temp1 (temp0 starts as 0)
<[>>>+<<<-]

      Outer loop runs temp1 (originally x) times; each iteration adds y to x
>>>
[
  <<
  [<+ >>+ <-]           Add y to x and to temp0
  >
  [<+ >-]               Restore y from temp0

  >-                    Decrement temp1
]


      Now the pointer is at temp1; both temp0 and temp1 are zeros;
      Add 8*8 to x by using a nested loop in temp0 and temp1; x was
      originally 20 so this loop will bring it to 84

++++++++
[<++++++++[<<+>>-]>-]

<<<.                    Print x; 84 is capital T
