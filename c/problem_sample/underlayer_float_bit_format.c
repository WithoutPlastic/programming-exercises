/*
 * Problem:
 * forced type convert reflect the underlayer float and integers bit format.
 *
 * Analysis:
 * integer format
 * 0x80000000 -> -2^32
 * 0xFFFFFFFF -> -1
 * 0x00000000 -> 0
 * 0x00000001 -> 1
 * 0x7FFFFFFF -> 2^32 - 1
 *
 * float format
 * 1bit 7bits    23bits
 * 0    10000010 10010000000000000000000
 * + 127 + 3  1100.1 -> 1.1001 -> 1001
 *
 * 01000001010010000000000000000000
 * 1095237632
 */

#include <stdio.h>

int main() {
    float a = 12.5;

    printf("%f\n", a);
    printf("%d\n", *(int *)&a);

    return 0;
}
