/*
 * Problem:
 * bit operator priority
 *
 * Analysis:
 * bit operator priority is higher the add operator. and it apply immediately.
 */

#include <stdio.h>

#define PrintInt(expr) printf("%s : %d\n", #expr, (expr))

int FiveTimes(int a) {
    return a << 2 + a;
}

int main() {
    PrintInt(FiveTimes(1));

    return 0;
}
