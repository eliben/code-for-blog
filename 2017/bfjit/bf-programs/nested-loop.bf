[
  Three loop bounds: i,j,k live at c0,c1,c2 accordingly. A triple loop runs and
  increments the target (at c3).
]

                Set i
+++++ +++++ +++++ +++++ +++++

[
                Set j for the next iteration of the inner loop
> +++++ +++++ +++++ +++++ +++++ +++++

[
                Set k for the next iteration of the innermost loop
> +++++ +++++ +++++ +++++ +++++ +++++ +++++

[               This is the innermost loop
>+<             Point to c3; increment it; then point back to c2
-
]

< -             Decrement j and check if the inner loop iterates again
]

< -             Decrement i and check if outer loop iterates again
]
