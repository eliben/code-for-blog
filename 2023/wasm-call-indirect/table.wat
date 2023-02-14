;; Sample of indirect calls in WASM.
;;
;; Eli Bendersky [https://eli.thegreenplace.net]
;; This code is in the public domain.

(module
    ;; The common type we use throughout the sample.
    (type $int2int (func (param i32) (result i32)))

    ;; Import a function named jstimes3 from the environment and call it
    ;; $jstimes3 here.
    (import "env" "jstimes3" (func $jstimes3 (type $int2int)))

    ;; Simple function that adds its parameter to itself and returns the sum.
    (func $wasmtimes2 (type $int2int)
        (i32.add (local.get 0) (local.get 0))
    )

    ;; Declare the dispatch function table to have two slots, and populate them
    ;; with functions. This uses the WASMv1 default table 0.
    (table $tbl 2 funcref)
    (elem (i32.const 0) $wasmtimes2 $jstimes3)

    ;; The following two functions are exported to JS; when JS calls them, they
    ;; invoke functions from the table.

    (func (export "times2") (type $int2int)
        ;; Place the value of the first parameter on the stack for call_indirect
        local.get 0

        ;; This call_indirect invokes a function of the given type from table at
        ;; offset 0. The parameters to this function are expected to be on
        ;; the stack.
        (call_indirect (type $int2int) (i32.const 0))
    )

    (func (export "times3") (type $int2int)
        ;; This is the same as times2, except it takes the function to call from
        ;; offset 1 in the table.
        local.get 0
        (call_indirect (type $int2int) (i32.const 1))
    )
)