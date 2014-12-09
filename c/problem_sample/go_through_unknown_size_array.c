/*
 * Problem:
 * there is no any output, not the 16 as you think.
 * 
 * Analysis:
 * the initial value of d is -1, which is signed int type. when do plus
 * operation with unsigned int, returned by sizeof. the type convert
 * signed-int->unsigned-int is performed. the result is huge. then you won't
 * see any output.
 */

#include <stdio.h>

#define TOTAL_ELEMENTS (sizeof(array) / sizeof(array[0]))

int array[] = {23,34,12,17,204,99,16};

int main() {
    for (int d = -1; d <= (TOTAL_ELEMENTS - 2); d++) {
        printf("%d\n", array[d + 1]);
    }

    return 0;
}
