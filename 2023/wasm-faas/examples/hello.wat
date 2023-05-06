;; Shows how to use WASI in WAT to print to stdout with fd_write.
;;
;; Eli Bendersky [https://eli.thegreenplace.net]
;; This code is in the public domain.
(module
    (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))

    ;; According to the WASI ABI, all WASM modules accessing WASI APIs have to
    ;; export a linear memory with the name "memory". Data pointers in WASI API
    ;; calls are relative to this memory's index space.
    ;; Declare our module's memory (starting with one page) and export it .
    (memory (export "memory") 1)

    ;; store a string at address 8
    (data (i32.const 8) "hello from wat!\n")

    (func $main (export "_start")
        ;; According to the wasi-libc API (https://tinyurl.com/4dtj78dp)
        ;; the parameters of fd_write are:
        ;;
        ;;  i32: file descriptor (1 for stdout)
        ;;  i32: address of array of vectors from which data is taken
        ;;  i32: length of this array
        ;;  i32: address of where to write result

        ;; Data vector, we only have one. It consists of two words: the address
        ;; of our string, and its length.
        (i32.store (i32.const 0) (i32.const 8))
        (i32.store (i32.const 4) (i32.const 16))

        ;; fd_write invocation:
        ;;   fd=1
        ;;   data vectors start at 0, and have length 1
        ;;   write result to 100
        (call $fd_write
            (i32.const 1)
            (i32.const 0)
            (i32.const 1)
            (i32.const 100)
        )
        drop
    )
)