/*
 * Problem:
 * how the marco is expanded with operator # and ##.
 * 
 * Analysis:
 * ## -> token-pasting operator
 * # -> stringizing operator
 * the macro-expanded is recursively. the expansion sequence need pay
 * attention. the preprocessor do it sequencely.
 */

#include <stdio.h>

#define f(a,b) a##b
#define g(a)   #a
#define h(a)   g(a)

int main() {                   //V             V             V   V
    printf("%s\n", h(f(1,2))); //h(f(1,2)) > g(f(1,2)) > g(12) > g(12) > "12"
    printf("%s\n", g(f(1,2))); //g(f(1,2)) > "f(1,2)"
                               //^
    return 0;
}
