#include <cstdio>
#include <cstring>
#include "wchashtable.h"
#include "wordsmap.h"

int main(int argc, char **argv) try
{
    bool isDump = false;
    bool onlySaveDump = false;
    const char *filename = NULL;

    switch (argc)
    {
    case 1:
        printf("Usage: %s [-d|-l] FILE\nPrint word counts for FILE\n%s%s", argv[0],
               "   -d  just save the DUMP file. Nothing to analyze\n",
               "   -l  LOAD the input file as a dump and analyze its\n");
        return 1;
    case 2:
        filename     = argv[1];
        break;
    default:
        onlySaveDump = !strcmp(argv[1], "-d");
        isDump       = !strcmp(argv[1], "-l");
        filename     = argv[2];
    }

    WordsMap wmap(filename, isDump);
    if (onlySaveDump)
    {
        char *fname = new char[strlen(filename) + 10];
        strcpy(fname, filename);
        strcat(fname, ".wordsmap");
        wmap.dump(fname);
        delete[] fname;
    }
    else
    {
        WCHachTable hashTable;
        char *currentWord = wmap.getWord();
        while (*currentWord)
        {
            hashTable.put(currentWord);
            currentWord = wmap.getWord();
        }

        hashTable.printResult();
        printf("Load factor: %.3lf\n", hashTable.getLoadFactor());
        printf("Average filling chains: %.3lf\n", hashTable.getAvrFillingTrees());
        printf("Maximum filling chains: %.3lf\n", hashTable.getMaxFillingTrees());
    }

    return 0;
}

catch (FileNotFoundException)
{
    fprintf(stderr, "E: file not found\n");
    return 1;
}
catch (WordBSTOverflowExceptionToManyWords)
{
    fprintf(stderr, "E: chain hash table overflowing\n");
    return 2;
}
