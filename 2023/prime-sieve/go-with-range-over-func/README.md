Go translations of the Python infinite sieve, using the experimental
[range over func proposal](https://github.com/golang/go/issues/61405).

To run these, use `gotip` per instructions in the proposal. Note that I
don't have a `go.mod` here, to avoid issues with toolchain updates and
`gotip` (the proposal has a race with the forward compatibility work
landing in 1.21)

To test:

```
gotip test -bench=. genprime_test.go genprime.go
```
