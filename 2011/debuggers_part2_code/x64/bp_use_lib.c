/* Code sample: Use debuglib for setting breakpoints in a child process.
**
** Eli Bendersky (http://eli.thegreenplace.net)
** This code is in the public domain.
*/
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <signal.h>
#include <syscall.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/reg.h>
#include <sys/user.h>
#include <unistd.h>
#include <errno.h>

#include "debuglib.h"


void run_debugger(pid_t child_pid, uint64_t addr)
{
    procmsg("debugger started\n");

    /* Wait for child to stop on its first instruction */
    wait(0);
    procmsg("child now at EIP = %p\n", get_child_eip(child_pid));

    /* Create breakpoint and run to it*/
    debug_breakpoint* bp = create_breakpoint(child_pid, (void*) addr);
    procmsg("breakpoint created\n");
    ptrace(PTRACE_CONT, child_pid, 0, 0);
    wait(0);

    /* Loop as long as the child didn't exit */
    while (1) {
        /* The child is stopped at a breakpoint here. Resume its
        ** execution until it either exits or hits the
        ** breakpoint again.
        */
        procmsg("child stopped at breakpoint. EIP = %p\n", get_child_eip(child_pid));
        procmsg("resuming\n");
        int rc = resume_from_breakpoint(child_pid, bp);

        if (rc == 0) {
            procmsg("child exited\n");
            break;
        }
        else if (rc == 1) {
            continue;
        }
        else {
            procmsg("unexpected: %d\n", rc);
            break;
        }
    }

    cleanup_breakpoint(bp);
}


int main(int argc, char** argv)
{
    pid_t child_pid;

    if (argc < 3) {
        fprintf(stderr, "Expected <program name> <breakpoint address as hex string> as argument\n");
        return -1;
    }

    child_pid = fork();
    if (child_pid == 0)
        run_target(argv[1]);
    else if (child_pid > 0) {
        uint64_t addr = (uint64_t) strtol(argv[2], NULL, 16);
        run_debugger(child_pid, addr);
    }
    else {
        perror("fork");
        return -1;
    }

    return 0;
}
