/* Code sample: simplistic "library" of debugging tools.
**
** Eli Bendersky (http://eli.thegreenplace.net)
** This code is in the public domain.
*/
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/reg.h>
#include <sys/user.h>
#include <unistd.h>
#include <errno.h>

#include "debuglib.h"


/* Print a message to stdout, prefixed by the process ID
*/
void procmsg(const char* format, ...)
{
    va_list ap;
    fprintf(stdout, "[%d] ", getpid());
    va_start(ap, format);
    vfprintf(stdout, format, ap);
    va_end(ap);
}


/* Run a target process in tracing mode by exec()-ing the given program name.
*/
void run_target(const char* programname)
{
    procmsg("target started. will run '%s'\n", programname);

    /* Allow tracing of this process */
    if (ptrace(PTRACE_TRACEME, 0, 0, 0) < 0) {
        perror("ptrace");
        return;
    }

    /* Replace this process's image with the given program */
    execl(programname, programname, 0);
}


long get_child_eip(pid_t pid)
{
    struct user_regs_struct regs;
    ptrace(PTRACE_GETREGS, pid, 0, &regs);
    return regs.eip;
}


void dump_process_memory(pid_t pid, unsigned from_addr, unsigned to_addr)
{
    procmsg("Dump of %d's memory [0x%08X : 0x%08X]\n", pid, from_addr, to_addr);
    for (unsigned addr = from_addr; addr <= to_addr; ++addr) {
        unsigned word = ptrace(PTRACE_PEEKTEXT, pid, addr, 0);
        printf("  0x%08X:  %02x\n", addr, word & 0xFF);
    }
}


/* Encapsulates a breakpoint. Holds the address at which the BP was placed
** and the original data word at that address (prior to int3) insertion.
*/
struct debug_breakpoint_t {
    void* addr;
    unsigned orig_data;
};


/* Enable the given breakpoint by inserting the trap instruction at its 
** address, and saving the original data at that location.
*/
static void enable_breakpoint(pid_t pid, debug_breakpoint* bp)
{
    assert(bp);
    bp->orig_data = ptrace(PTRACE_PEEKTEXT, pid, bp->addr, 0);
    ptrace(PTRACE_POKETEXT, pid, bp->addr, (bp->orig_data & 0xFFFFFF00) | 0xCC);
}


/* Disable the given breakpoint by replacing the byte it points to with
** the original byte that was there before trap insertion.
*/
static void disable_breakpoint(pid_t pid, debug_breakpoint* bp)
{
    assert(bp);
    unsigned data = ptrace(PTRACE_PEEKTEXT, pid, bp->addr, 0);
    assert((data & 0xFF) == 0xCC);
    ptrace(PTRACE_POKETEXT, pid, bp->addr, (data & 0xFFFFFF00) | (bp->orig_data & 0xFF));
}


debug_breakpoint* create_breakpoint(pid_t pid, void* addr)
{
    debug_breakpoint* bp = malloc(sizeof(*bp));
    bp->addr = addr;
    enable_breakpoint(pid, bp);
    return bp;
}


void cleanup_breakpoint(debug_breakpoint* bp)
{
    free(bp);
}


int resume_from_breakpoint(pid_t pid, debug_breakpoint* bp)
{
    struct user_regs_struct regs;
    int wait_status;

    ptrace(PTRACE_GETREGS, pid, 0, &regs);
    /* Make sure we indeed are stopped at bp */
    assert(regs.eip == (long) bp->addr + 1);

    /* Now disable the breakpoint, rewind EIP back to the original instruction
    ** and single-step the process. This executes the original instruction that
    ** was replaced by the breakpoint.
    */
    regs.eip = (long) bp->addr;
    ptrace(PTRACE_SETREGS, pid, 0, &regs);
    disable_breakpoint(pid, bp);
    if (ptrace(PTRACE_SINGLESTEP, pid, 0, 0) < 0) {
        perror("ptrace");
        return -1;
    }
    wait(&wait_status);

    if (WIFEXITED(wait_status)) {
        return 0;
    }

    /* Re-enable the breakpoint and let the process run.
    */
    enable_breakpoint(pid, bp);

    if (ptrace(PTRACE_CONT, pid, 0, 0) < 0) {
        perror("ptrace");
        return -1;
    }
    wait(&wait_status);

    if (WIFEXITED(wait_status))
        return 0;
    else if (WIFSTOPPED(wait_status)) {
        return 1;
    }
    else
        return -1;
}


