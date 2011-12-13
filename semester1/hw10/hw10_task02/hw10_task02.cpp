#include <cstdio>
#include "finite_state_machine.h"

int main(void)
{
    printf("Enter a double value: ");
    char buf[80];
    scanf("%s", buf);

    if (run(buf))
        printf("OK\n");
    else
        printf("FAIL\n");

    return 0;
}
