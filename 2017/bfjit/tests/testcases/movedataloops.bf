>+                          Set c1 = 1
>++                         Set c2 = 2
>+++                        Set c3 = 3
>++++                       Set c4 = 4

[->>>>>>>>+<<<<<<<<]        Move the value of c4 to c12

<
[->>>>>>>>>+<<<<<<<<<]      Add the value of c3 to c12


                            Print the value of c12 as a single ASCII digit
>>>>>>>>>
++++++++ ++++++++ ++++++++ ++++++++ ++++++++ ++++++++.

<
[->+<]                      Add the value of c11 (which is 0) to c12

                            Print the value of c12 as a single ASCII digit
>.

<<<<<<<<<<<
[->+<]                      Add the value of c1 to c2 and print c2
>
++++++++ ++++++++ ++++++++ ++++++++ ++++++++ ++++++++.

>
[-<+>]                      Add the value of c3 (which is 0) to c2 nd print c2
<.

>+++                        Set c3 to 3 and add its value to c2 again and print c2
[-<+>]
<.


Now in a loop set c3 to 2 and add it with a move loop to c6

[-]						 	c2 is the loop counter initialized to 4
++++

[
  >++						set c3 to 2
  [->>>+<<<]                add c3 to c6
  <-                        decrement loop counter
]

>>>>                        Print c6
++++++++ ++++++++ ++++++++ ++++++++ ++++++++ ++++++++.

Similarly in a loop do the same but now move data left from c6 to c4

<<<<
[-]						 	c2 is the loop counter initialized to 4
++++

[
  >>>>[-]++
  [-<<+>>]
  <<<<-
]

>>                          Print c4
++++++++ ++++++++ ++++++++ ++++++++ ++++++++ ++++++++.


