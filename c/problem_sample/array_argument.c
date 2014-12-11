/*
 * Problem:
 * the array, pointer, offset in a defined array
 *
 * Analysis:
 * top of array always be a pointer.
 * the [] operator indeed do offset stuff on pointer.
 */

#include <stdio.h>

#define SIZE 10

void size(int arr[SIZE][SIZE]) {
    printf("%zu %zu\n", sizeof(arr), sizeof(arr[0]));
}

int main() {
    int arr[SIZE][SIZE];

    size(arr);

    return 0;
}
