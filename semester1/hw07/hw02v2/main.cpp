#include <cstdio>
#include "wordcounter.v2.h"

int main(void)
{
    WordBST * tree = new WordBST;
    unsigned int i = 0;
    for (; i < sizeof(WordBST); i++)
        ((char *) tree)[i] = '*';
    tree->next = NULL;
    tree->top = 0;

    printf("%d\n", append(tree, "i"));
    printf("%d\n", handle(tree, "am"));
    printf("%d\n", handle(tree, "programming"));
    printf("%d\n", handle(tree, "am"));


    return 0;
}
