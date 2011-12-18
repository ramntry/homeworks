#include "polynomial.h"
#include <cstdio>

void opAdd(Polynomial *space)
{
    int degree = 0;
    char name = '"';
    scanf("%*[ \n\t\r]%c%d", &name, &degree);

    if (name >= 'a' && name <= 'z' && degree >= 0)
    {
        space[name - 'a'].clear();
        fillPolynomial(space[name - 'a'], degree);
    }
}

void opPrint(Polynomial *space)
{
    char name = '"';
    scanf("%*[ \n\t\r]%c", &name);
    if (name >= 'a' && name <= 'z')
        printPolynomial(space[name - 'a']);
}

void opRemove(Polynomial *space)
{
    char name = '"';
    scanf("%*[ \n\t\r]%c", &name);
    if (name >= 'a' && name <= 'z')
        space[name - 'a'].clear();
}

void opIsEqual(Polynomial *space)
{
    char namel = '"';
    char namer = '"';
    scanf("%*[ \n\t\r]%c%*[ \n\t\r]%c", &namel, &namer);
    if ((namel >= 'a' && namel <= 'z') && (namer >= 'a' && namer <= 'z'))
    {
        if (isEqualPolynomial(space[namel - 'a'], space[namer - 'a']))
            printf("are Equal\n");
        else
            printf("are Not equal\n");
    }
}

void opSumm(Polynomial *space)
{
    char namel = '"';
    char namer = '"';
    char named = '"';
    scanf("%*[ \n\t\r]%c%*[ \n\t\r]%c%*[ \n\t\r]%c", &named, &namel, &namer);
    if ((namel >= 'a' && namel <= 'z') && (namer >= 'a' && namer <= 'z') && (named >= 'a' && named <= 'z'))
    {
        if (named == namel || named == namer)
            printf("Destination not be the same with one of the arguments\n");
        else
        {
            space[named - 'a'].clear();
            addPolynomial(space[named - 'a'], space[namel - 'a'], space[namer - 'a']);
        }
    }
}

void opValue(Polynomial *space)
{
    int point = 0;
    char name = '"';
    scanf("%*[ \n\t\r]%c%d", &name, &point);

    if (name >= 'a' && name <= 'z' && space[name - 'a'].head != NULL)
        printf("%d\n", valuePolynomial(space[name - 'a'], point));
}

void opHelp()
{
    printf("Using:\n    <command (or only first letter)> [<argument_1> ...]\n\n"
           "    add   <one letter name> <degree> <coefficients...>\n"
           "          - add a new polynomial in the system\n"
           "    rem   <name>         - remove the polynomial from the system\n"
           "    print <name>         - print the polynomial\n"
           "    equal <name> <name>  - test an equality\n"
           "    summ  <destination name> <arg1> <arg2> - addition\n"
           "    val   <name> <x>     - calculate a value in a point\n"
           "    q     - exit\n"
           "    h     - print this help message\n"
           );
}

int main()
{
    printf("This program performs some basic operations with polynomials.\n"
           "Type help (or h) for more information\n");

    Polynomial space['z' - 'a' + 1];

    char command[20];
    for (;;)
    {
        printf("> ");
        scanf("%s", command);
        switch (command[0])
        {
        case 'a':
            opAdd(space);
            break;
        case 'p':
            opPrint(space);
            break;
        case 'r':
            opRemove(space);
            break;
        case 'e':
            opIsEqual(space);
            break;
        case 's':
            opSumm(space);
            break;
        case 'v':
            opValue(space);
            break;
        case 'h':
            opHelp();
            break;
        case 'q':
            return 0;
        }
    }
}
