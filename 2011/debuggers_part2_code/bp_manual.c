/* Code sample: manual setting of a breakpoint, using ptrace
**
** Eli Bendersky (http://eli.thegreenplace.net)
** This code is in the public domain.
*/
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
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


/* For some reason this declaration isn't available from string.h */
extern char* strsignal(int);


void run_debugger(pid_t child_pid, long addr)
{
    int wait_status;
    struct user_regs_struct regs;
    procmsg("debugger started\n");

    /* Wait for child to stop on its first instruction */
    wait(&wait_status);

    /* Obtain and show child's instruction pointer */
    ptrace(PTRACE_GETREGS, child_pid, 0, &regs);
    procmsg("Child started. EIP = %p\n", regs.rip);

    /* Look at the word at the address we're interested in */
    long data = ptrace(PTRACE_PEEKTEXT, child_pid, (void*)addr, 0);
    procmsg("Original data at %p: %p\n", addr, data);

    /* Write the trap instruction 'int 3' into the address */
    long data_with_trap = (data & ~0xFF) | 0xCC;
    ptrace(PTRACE_POKETEXT, child_pid, (void*)addr, (void*)data_with_trap);

    /* See what's there again... */
    long readback_data = ptrace(PTRACE_PEEKTEXT, child_pid, (void*)addr, 0);
    procmsg("After trap, data at %p: %p\n", addr, readback_data);

    /* Let the child run to the breakpoint and wait for it to
    ** reach it 
    */
    ptrace(PTRACE_CONT, child_pid, 0, 0);

    wait(&wait_status);
    if (WIFSTOPPED(wait_status)) {
        procmsg("Child got a signal: %s\n", strsignal(WSTOPSIG(wait_status)));
    }
    else {
        perror("wait");
        return;
    }

    /* See where the child is now */
    ptrace(PTRACE_GETREGS, child_pid, 0, &regs);
    procmsg("Child stopped at EIP = %p\n", regs.rip);

    /* Remove the breakpoint by restoring the previous data
    ** at the target address, and unwind the EIP back by 1 to 
    ** let the CPU execute the original instruction that was 
    ** there.
    */
    ptrace(PTRACE_POKETEXT, child_pid, (void*)addr, (void*)data);
    regs.rip -= 1;
    ptrace(PTRACE_SETREGS, child_pid, 0, &regs);

    /* The child can continue running now */
    ptrace(PTRACE_CONT, child_pid, 0, 0);

    wait(&wait_status);

    if (WIFEXITED(wait_status)) {
        procmsg("Child exited\n");
    } 
    else {
        procmsg("Unexpected signal\n");
    }
}


int main(int argc, char** argv)
{
    pid_t child_pid;

    if (argc < 3) {
        fprintf(stderr, "Expected <program name> and <breakpoint address as hex string> as argument\n");
        return -1;
    }

    child_pid = fork();
    if (child_pid == 0)
        run_target(argv[1]);
    else if (child_pid > 0) {
        long addr = strtol(argv[2], NULL, 16);
        run_debugger(child_pid, addr);
    }
    else {
        perror("fork");
        return -1;
    }

    return 0;
}
