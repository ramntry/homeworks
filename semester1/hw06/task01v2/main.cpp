#include <cstdio>
#include "binarysearchtree.h"

int getItem()
{
    int item = 0;
    if (scanf("%d", &item))
        return item;
    scanf("%*s");
    printf("Incorrect item. Must be integer. Try again: ");
    return getItem();
}

int main(void)
{
    printf("Set of numbers. Type h for more information\n");
    NumbersSet s;
    char buf[80];
    for (;;)
    {
        printf("> ");
        scanf("%s", buf);
        switch (buf[0])
        {
        case 'a': // add
            s.add(getItem());
            break;
        case 'd': // delete
            s.del(getItem());
            break;
        case 'i': // in (has)
            if (s.has(getItem()))
                printf("yes\n");
            else
                printf("no\n");
            break;
        case 'p': // print
            s.print();
            break;
        case 'r': // reverse print
            s.print(true);
            break;
        case 'h': // help
            printf("Using:\n"
                 "    <command> [argument]\n\n"
                 "    add <item> - insert the element in the Set\n"
                 "    del <item> - exclude the element from the Set\n"
                 "    in  <item> - check the element in the Set\n"
                 "    print      - print the Set\n"
                 "    reverse    - print in back order\n"
                 "    q          - exit\n"
                 "    h          - print this help\n");
            break;
        case 'q': // exit
            return 0;
        default:
            printf("Unknown command. Type h for more information\n");
        }
    }
}
