[
  Tests of various optimizable loops
]

>>>>>>                      Set c6 = 5
+++++

>>>>>>                      Set c12 = 5
+++++

<<<<<<

[-<<<+>>>]                  Move the value of c6 to c3


<+++                        Set c5 = 3

[-<<+>>]                    Add c5 to c3 (so add 3 to 5 in c3)

<<
                            Print the value of c3 as a single ASCII digit
++++++++ ++++++++ ++++++++ ++++++++ ++++++++ ++++++++.

[-]                         Clear c3

                            Print the value of c3 as a single ASCII digit
++++++++ ++++++++ ++++++++ ++++++++ ++++++++ ++++++++.

                            Set values in c3 to c10 to nonzero

>+>+>+>+>+>+>+

                            Hop back to the first zero field before c10

[<<]
                            Print its (c2) value of as a single ASCII digit
++++++++ ++++++++ ++++++++ ++++++++ ++++++++ ++++++++.