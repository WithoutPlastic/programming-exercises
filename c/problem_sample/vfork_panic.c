/*
 * Problem:
 * why in vfork children process, call return will lead whole program failure.
 *
 * Analysis:
 * fork() create a children process, and copy father process memory data to
 * itself. vfork() create a children process, and share memory data with father
 * process. and vfork will give promise that, children process is executed
 * first, when children process call exit() or exec(), father process then 
 * continue. please see man page of vfork if you are not sure.
 *
 * and then, why vfork is created? due to historical reason, program always
 * fork a process and run exec() to outside program. hence, the memory copy is
 * needless. so BSD first to create vfork with little overhead, and dedicated
 * for exec usage.
 *
 * to end a process, use exit() instead of return. if you return in vfork
 * children process, it means that main() returned. generially, _exit() or
 * something similar called. since memory is shared, whole program end
 * unexpectedly. but exit() work well. it won't change stack. father process
 * can run normally.
 *
 * finally, you see that vfork is so dangerous, programer improve fork with
 * copy-on-write(COW), which reduce the fork overhead.
 *
 * and when children process return in main function. newer kernel will bring
 * up main function again.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

//#define _BSD_SOURCE

int glob = 6;

int main(void) {
    int var;
    pid_t pid;

    var = 88;
    if ((pid = vfork()) < 0) {
        printf("vfork error.");
        exit(-1);
    } else if (pid == 0) { /* childred process */
        glob++; /* modify parent's variable */
        var++;
        return 0; /* normally use exit(0) */
    }
    /* parent continue here. */

    printf("pid=%d, glob=%d, var=%d\n", getpid(), glob, var);
    return 0; 
}
