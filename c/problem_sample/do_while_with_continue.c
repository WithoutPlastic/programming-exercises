/*
 * Problem:
 * not sure the continue statement jump to, either for loop or do while loop.
 *
 * Analysis:
 * check the C manual youself, for loop will go to the third statement of for
 * header. and do while will go to condition statement.
 */

#include <stdio.h>
#include <stdbool.h>

int main() {
    int i = 1;

    do {
        printf("%d\n", i);
        i++;
        if (i < 15) {
            continue;
        }
    } while(false);

    return 0;
}
