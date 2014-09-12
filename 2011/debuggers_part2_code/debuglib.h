/* Code sample: simplistic "library" of debugging tools.
**
** Eli Bendersky (http://eli.thegreenplace.net)
** This code is in the public domain.
*/
#include <sys/types.h>


/* Print out a message, prefixed by the process ID.
*/
void procmsg(const char* format, ...);


/* Run the given program in a child process with exec() and tracing
** enabled.
*/
void run_target(const char* programname);


/* Retrieve the child process's current instruction pointer value.
*/
long get_child_eip(pid_t pid);


/* Display memory contents in the inclusive range [from_addr:to_addr] from the
** given process's address space.
*/
void dump_process_memory(pid_t pid, unsigned from_addr, unsigned to_addr);


struct debug_breakpoint_t;
typedef struct debug_breakpoint_t debug_breakpoint;


/* Create a breakpoint for the child process pid, at the given address.
*/
debug_breakpoint* create_breakpoint(pid_t pid, void* addr);


/* Clean up the memory allocated for the given breakpoint.
** Note: this doesn't disable the breakpoint, just deallocates it.
*/
void cleanup_breakpoint(debug_breakpoint* bp);


/* Given a process that's currently stopped at breakpoint bp, resume
** its execution and re-establish the breakpoint.
** Return 0 if the process exited while running, 1 if it has stopped 
** again, -1 in case of an error.
*/
int resume_from_breakpoint(pid_t pid, debug_breakpoint* bp);


