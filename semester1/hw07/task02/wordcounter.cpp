#include "wordcounter.h"
#include <cstring>
#include <cstdio>

inline int to4(int address)
{
    int mask = 0;
    mask = ~mask;
    mask <<= 2;
    return (address + 1) / 4 * 4 + 2;
}

int WordNodeInit(char *place, char *word)
{
    WordNode * node = (WordNode *) place;
    node->counter = 1;
    node->leftChild = 0;
    node->rightChild = 0;
    int i = 0;
    for (; ((&(node->firstLetter))[i] = word[i]); i++);
    return ++i;
}

char *WordBSTInit(char *place, char *word)
{
    WordBST * bst = (WordBST *) place;
    bst->next = NULL;
    bst->current = WordNodeInit(place + wordBSTHeaderSize, word);
    return place + fullHeaderSize;
}

int strCmp(char * fst, char * snd)
{
        int i = 0;
        for (; fst[i] && fst[i] == snd[i]; i++);
        if (fst[i] > snd[i])
                return 1;
        if (fst[i] < snd[i])
                return -1;
        return 0;
}

void addWord(char *place, char *word)
{
    char *current = place;
    char *parent = NULL;
    int resCmp = 0;
    for (;;)
    {
        resCmp = strCmp(word, current);
        if (resCmp > 0)
        {
            parent = current - 1;
            if (*parent)
                current = place + *parent + wordNodeHeaderSize;
            else
                break;
        }
        else if (resCmp < 0)
        {
            parent = current - 2;
            if (*parent)
                current = place + *parent + wordNodeHeaderSize;
            else
                break;
        }
        else
            break;
    }
    if (resCmp)
    {
        WordBST * bst = (WordBST *) (place - fullHeaderSize);
        if (bst->current + wordNodeHeaderSize + strlen(word) >= wordBSTSize)
            throw OverflowException();
        *parent = bst->current;
        bst->current += WordNodeInit(place + bst->current, word)
                      + wordNodeHeaderSize;
    }
    else
    {
        WordNode * node = (WordNode *) (current - wordNodeHeaderSize);
        node->counter++;
    }
}

void printWordBST(char *current, char *hashTable)
{
    if (!hashTable)
        hashTable = current;
    int leftChild = *(current - 2);
    int rightChild = *(current - 1);

    if (leftChild)
        printWordBST(hashTable + leftChild + wordNodeHeaderSize, hashTable);
    printf("%d -\t%s\n", *((int *) (current - wordNodeHeaderSize)), current);
    if (rightChild)
        printWordBST(hashTable + rightChild + wordNodeHeaderSize, hashTable);
}
