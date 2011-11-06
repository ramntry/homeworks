#include <cstdio>
#include <cstring>
#include "wchashtable.h"
#include "wordsmap.h"

inline bool log(const char *message) { printf(message); return fflush(stdout); }

bool getArgs(int argc, char **argv, char *filename, bool &isDump, bool &onlySaveDump, bool &silentMode)
{
    switch (argc)
    {
    case 1:
        printf("Usage: %s [-d|-l|-s] FILE\nPrint word counts for FILE\n%s%s%s", argv[0],
               "   -d  just save the DUMP file. Nothing to analyze\n",
               "   -l  LOAD the input file as a dump and analyze its\n",
               "   -s  SILENT mode - print only results\n");

        return false;
    case 2:
        strcpy(filename,       argv[1]);
        return true;
    case 3:
        onlySaveDump = !strcmp(argv[1], "-d");
        isDump       = !strcmp(argv[1], "-l");
        silentMode   = !strcmp(argv[1], "-s");
        strcpy(filename,       argv[2]);
        return true;
    default:
        onlySaveDump = !strcmp(argv[1], "-d") || !strcmp(argv[2], "-d");
        isDump       = !strcmp(argv[1], "-l") || !strcmp(argv[2], "-l");
        silentMode   = !strcmp(argv[1], "-s") || !strcmp(argv[2], "-s");
        strcpy(filename,       argv[3]);
        return true;
    }
}

int main(int argc, char **argv) try
{
    bool isDump = false;
    bool onlySaveDump = false;
    bool silentMode = false;
    char *filename = new char[268];
    filename[0] = '\0';
    if(!getArgs(argc, argv, filename, isDump, onlySaveDump, silentMode))
        return 1;

    silentMode || log("Loading...");
    WordsMap wmap(filename, isDump);

    if (onlySaveDump)
    {
        strcat(filename, ".wordsmap");
        wmap.dump(filename);
        silentMode || log(" dump [OK]\n");
    }
    else
    {
        silentMode || log("\nCounting..");
        WCHachTable hashTable;

        int counter = 0;
        char *currentWord = wmap.getWord();
        while (*currentWord)
        {
            hashTable.put(currentWord);
            currentWord = wmap.getWord();

            counter++;
            if (!silentMode && counter % 180000 == 0)   // Приблизительно одна точка на мегабайт
                log(".");
        }
        silentMode || log("      [OK]\n\n");

/*        unsigned int uniqWords = hashTable.printResult();
        if (!silentMode)
        {
            printf("\nTotal: %u words about %.1lf letters each"
                   "\n       with average frequency %.3lf\n\n",
                   counter,
                  (double) (wmap.getSize() - 4) / counter - 1,
                  (double) counter / uniqWords);
            hashTable.printStat();
        } */
    }
    delete[] filename;

    return 0;
}

catch (FileNotFoundException)
{
    fprintf(stderr, "E: file not found\n");
    return 1;
}
catch (WordBSTOverflowTooManyWordsException)
{
    fprintf(stderr, "E: chain hash table overflowing\n");
    return 2;
}
