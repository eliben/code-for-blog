;; Simple `itoa` implementation in WebAssembly Text.
;;
;; Eli Bendersky [https://eli.thegreenplace.net]
;; This code is in the public domain.
(module
    ;; Logging function imported from the environment; will print a single
    ;; i32.
    (import "env" "log" (func $log (param i32)))

    ;; Declare linear memory and export it to host. The offset returned by
    ;; $itoa is relative to this memory.
    (memory (export "memory") 1)

    ;; Using some memory for a number-->digit ASCII lookup-table, and then the
    ;; space for writing the result of $itoa.
    (data (i32.const 8000) "0123456789")
    (global $itoa_out_buf i32 (i32.const 8010))

    ;; itoa: convert an integer to its string representation. Only supports
    ;; numbers >= 0.
    ;; Parameter: the number to convert
    ;; Result: address and length of string in memory.
    ;; Note: this result is only valid until the next call to $itoa which will
    ;; overwrite it; obviously, this isn't concurrency-safe either.
    (func $itoa (export "itoa") (param $num i32) (result i32 i32)
        (local $numtmp i32)
        (local $numlen i32)
        (local $writeidx i32)
        (local $digit i32)
        (local $dchar i32)

        ;; Count the number of characters in the output, save it in $numlen.
        (i32.lt_s (local.get $num) (i32.const 10))
        if
            (local.set $numlen (i32.const 1))
        else
            (local.set $numlen (i32.const 0))
            (local.set $numtmp (local.get $num))
            (loop $countloop (block $breakcountloop
                (i32.eqz (local.get $numtmp))
                br_if $breakcountloop

                (local.set $numtmp (i32.div_u (local.get $numtmp) (i32.const 10)))
                (local.set $numlen (i32.add (local.get $numlen) (i32.const 1)))
                br $countloop
            ))
        end

        ;; Now that we know the length of the output, we will start populating
        ;; digits into the buffer. E.g. suppose $numlen is 4:
        ;;
        ;;                     _  _  _  _
        ;;
        ;;                     ^        ^
        ;;  $itoa_out_buf -----|        |---- $writeidx
        ;;  
        ;;
        ;; $writeidx starts by pointing to $itoa_out_buf+3 and decrements until
        ;; all the digits are populated.
        (local.set $writeidx 
            (i32.sub 
                (i32.add (global.get $itoa_out_buf) (local.get $numlen))
                (i32.const 1)))

        (loop $writeloop (block $breakwriteloop
            ;; digit <- $num % 10
            (local.set $digit (i32.rem_u (local.get $num) (i32.const 10)))
            ;; set the char value from the lookup table of digit chars
            (local.set $dchar (i32.load8_u offset=8000 (local.get $digit)))
            
            ;; mem[writeidx] <- dchar
            (i32.store8 (local.get $writeidx) (local.get $dchar))

            ;; num <- num / 10
            (local.set $num (i32.div_u (local.get $num) (i32.const 10)))

            ;; If after writing a number we see we wrote to the first index in
            ;; the output buffer, we're done.
            (i32.eq (local.get $writeidx) (global.get $itoa_out_buf))
            br_if $breakwriteloop

            (local.set $writeidx (i32.sub (local.get $writeidx) (i32.const 1)))
            br $writeloop
        ))

        ;; return (itoa_out_buf, numlen)
        (global.get $itoa_out_buf)
        (local.get $numlen)
    )
)
