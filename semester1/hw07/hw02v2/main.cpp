#include <cstdio>
#include <cstring>
#include "wchashtable.h"
#include "wordsmap.h"

inline void fl() { fflush(stdout); }

int main(int argc, char **argv) try
{
    bool isDump = false;
    bool onlySaveDump = false;
    char *filename = new char[268];
    filename[0] = '\0';

    switch (argc)
    {
    case 1:
        printf("Usage: %s [-d|-l] FILE\nPrint word counts for FILE\n%s%s", argv[0],
               "   -d  just save the DUMP file. Nothing to analyze\n",
               "   -l  LOAD the input file as a dump and analyze its\n");
        return 1;
    case 2:
        strcpy(filename,       argv[1]);
        break;
    default:
        onlySaveDump = !strcmp(argv[1], "-d");
        isDump       = !strcmp(argv[1], "-l");
        strcpy(filename,       argv[2]);
    }

    printf("Loading..."); fl();
    WordsMap wmap(filename, isDump);

    if (onlySaveDump)
    {
        strcat(filename, ".wordsmap");
        wmap.dump(filename);
        printf(" dump [OK]\n"); fl();
    }
    else
    {
        printf("\nCounting.."); fl();
        WCHachTable hashTable;

        int counter = 0;
        char *currentWord = wmap.getWord();
        while (*currentWord)
        {
/* Мега- */ hashTable.put(currentWord);
/* моло- */ currentWord = wmap.getWord();
/* тилка */
            counter++;
            if (counter % 250000 == 0)   // Одна точка на мегабайт - приблизительно
                putchar('.'); fl();
        }
        printf("      [OK]\n\n"); fl();

        unsigned int uniqWords = hashTable.printResult();
        printf("\nTotal: %u words about %.3lf letters each"
               "\n       with average frequency %.3lf\n\n",
               counter,
              (double) (wmap.getSize() - 4) / counter - 1,
              (double) counter / uniqWords);
        hashTable.printStat();
    }

    delete[] filename;

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
