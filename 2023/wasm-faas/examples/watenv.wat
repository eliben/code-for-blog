(module
    (import "env" "log_i32" (func $log_i32 (param i32)))
    (import "env" "log_string" (func $log_string (param i32 i32)))

    (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
    (import "wasi_snapshot_preview1" "environ_sizes_get" (func $environ_sizes_get (param i32 i32) (result i32)))
    (import "wasi_snapshot_preview1" "environ_get" (func $environ_get (param i32 i32) (result i32)))

    (memory (export "memory") 1)

    ;; These slots are used as parameters for fd_write, and its return value.
    (global $datavec_addr i32 (i32.const 100))
    (global $datavec_len i32 (i32.const 104))
    (global $fdwrite_ret i32 (i32.const 108))

    ;; Slots for the return values of $environ_sizes_get
    (global $env_count i32 (i32.const 600))
    (global $env_len i32 (i32.const 604))

    ;; Slots for the return values of $environ_get
    ;; Each pointer is 4 bytes, so this has enough spaces for 250 env vars,
    ;; which should be plenty.
    ;; $environ_sizes_get will tell use the number of env vars and their
    ;; total buffer length. We could use the latter to "allocate" space
    ;; before calling $environ_get, but for simplicity we simply
    ;; leave enough memory for it by spacing out allocations.
    (global $env_ptrs i32 (i32.const 1000))
    (global $env_buf i32 (i32.const 2000))

    (data (i32.const 750) "logging from watenv")
    (data (i32.const 800) "watenv environment:")
    (data (i32.const 850) "\n")

    (func $main (export "_start")
        (local $i i32)
        (local $num_of_envs i32)
        (local $next_env_ptr i32)

        (call $log_string (i32.const 750) (i32.const 19))

        ;; Find out the number of env vars.
        (call $environ_sizes_get (global.get $env_count) (global.get $env_len))
        drop

        ;; Comment-in for more debug/logging
        ;; (call $log_i32 (i32.load (global.get $env_count)))
        ;; (call $log_i32 (i32.load (global.get $env_len)))

        ;; Get the env vars themselves into memory.
        (call $environ_get (global.get $env_ptrs) (global.get $env_buf))
        drop

        ;; Print out the preamble
        (call $println (i32.const 800) (i32.const 19))

        ;; for i = 0; i != *env_count; i++
        ;;   show env var i
        (local.set $num_of_envs (i32.load (global.get $env_count)))
        (local.set $i (i32.const 0))
        (loop $envvar_loop (block $break_envvar_loop
            (i32.eq (local.get $i) (local.get $num_of_envs))
            (br_if $break_envvar_loop)

            ;; next_env_ptr <- env_ptrs[i*4]
            (local.set
                $next_env_ptr
                (i32.load (i32.add  (global.get $env_ptrs)
                                    (i32.mul (local.get $i) (i32.const 4)))))

            ;; print out this env var
            (call $show_env (local.get $next_env_ptr))

            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $envvar_loop)
        ))
    )

    ;; println prints a string to stdout using WASI.
    ;; It takes the string's address and length as parameters.
    (func $println (param $strptr i32) (param $len i32)
        ;; Print the string pointed to by $strptr first.
        ;;   fd=1
        ;;   data vector with the pointer and length
        (i32.store (global.get $datavec_addr) (local.get $strptr))
        (i32.store (global.get $datavec_len) (local.get $len))
        (call $fd_write
            (i32.const 1)
            (global.get $datavec_addr)
            (i32.const 1)
            (global.get $fdwrite_ret)
        )
        drop

        ;; Print out a newline.
        (i32.store (global.get $datavec_addr) (i32.const 850))
        (i32.store (global.get $datavec_len) (i32.const 1))
        (call $fd_write
            (i32.const 1)
            (global.get $datavec_addr)
            (i32.const 1)
            (global.get $fdwrite_ret)
        )
        drop
    )

    ;; show_env emits a single env var pair to stdout. envptr points to it,
    ;; and it's 0-terminated.
    (func $show_env (param $envptr i32)
        (local $i i32)
        (local.set $i (i32.const 0))

        ;; for i = 0; envptr[i] != 0; i++
        (loop $count_loop (block $break_count_loop
            (i32.eqz (i32.load8_u (i32.add (local.get $envptr) (local.get $i))))
            br_if $break_count_loop

            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            br $count_loop
        ))

        (call $println (local.get $envptr) (local.get $i))
    )
)
