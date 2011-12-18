#include <cstdio>
#include "floatexpressions.h"

int main()
{
    printf("Check an arithmetic expression       (float numbers, +, -, *, /)\n"
           "(break enter by EOF signal (ctrl+D)): ");

    if (floatExpression(stdin))
        printf("OK\n");
    else
        printf("FAIL\n");

    return 0;
}
