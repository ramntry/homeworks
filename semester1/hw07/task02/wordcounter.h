#pragma once
#include <cstdlib>

const int wordBSTHeaderSize = sizeof(int *) + sizeof(int);
const int wordBSTSize = wordBSTHeaderSize + 32;
const int wordNodeHeaderSize = sizeof(int) + 2;
const int fullHeaderSize = wordBSTHeaderSize + wordNodeHeaderSize;

class OverflowException {};

struct WordNode
{
    int counter;
    unsigned char leftChild;
    unsigned char rightChild;
    char firstLetter;
};

union WordBST
{
    struct
    {
        WordBST *next;
        int current;
    };
    unsigned char tree[wordBSTSize];

};

int WordNodeInit(char *place, char *word);
char *WordBSTInit(char *place, char *word);
void addWord(char *place, char *word);
void printWordBST(char *current, char *hashTable = NULL);
