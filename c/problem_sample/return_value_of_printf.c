/*
 * Problem:
 * what is the return value of printf? normally, we only use its side effects.
 *
 * Analysis:
 * from manual, the printf return value represent the length of the printed
 * string.
 */

#include <stdio.h>

int main() {
    int i = 43;

    printf("%d\n", printf ("%d", printf ("%d", i)));

    return 0;
}
