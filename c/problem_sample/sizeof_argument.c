/*
 * Problem:
 * the statement in sizeof argument work incorrect.
 *
 * Analysis:
 * the sizeof is not really called in running time. it is keyword for
 * compile-time. the compiler will determine the size, according to return
 * value.
 */

#include <stdio.h>

int main() {
    int i;

    i = 10;
    printf("i : %d\n", i);
    printf("sizeof(i++) is: %zu\n", sizeof(i++));
    printf("i : %d\n", i);
    
    return 0;
}
