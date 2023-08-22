Go translations of the Python infinite sieve, using the experimental
[range over func proposal](https://github.com/golang/go/issues/61405).

To run these, use `gotip` per instructions in the proposal, along with
`GOTOOLCHAIN=local` to always use the local toolchain.

```
gotip test -bench=. genprime_test.go genprime.go
```
