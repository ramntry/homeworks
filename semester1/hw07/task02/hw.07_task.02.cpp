#include "wordcounter.h"
#include <cstdio>
#include <cstdlib>

int main(void)
{
    char *place = (char *) malloc(wordBSTSize);
    for (int i = 0; i < wordBSTSize; i++)
        place[i] = '*';
    char word[] = "word";
    char word2[] = "hello";
    char word3[] = "abc";
    char word4[] = "zoom";
    char *hashTable = WordBSTInit(place, word);
    addWord(hashTable, word3);
    addWord(hashTable, word2);
    addWord(hashTable, word4);
    addWord(hashTable, word);
    addWord(hashTable, word3);
    addWord(hashTable, word2);
    addWord(hashTable, word4);
    addWord(hashTable, word);
    addWord(hashTable, word2);
    addWord(hashTable, word4);
    addWord(hashTable, word4);
    addWord(hashTable, word);
    addWord(hashTable, word4);
    addWord(hashTable, word2);
    addWord(hashTable, word);
    printWordBST(hashTable);
    return 0;
}
