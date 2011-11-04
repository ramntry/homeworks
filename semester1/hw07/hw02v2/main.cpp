#include <cstdio>
#include <cctype>
#include "wchashtable.h"

inline char *strToLower(char *buf)
{
    for (int i = 0; (buf[i] = tolower(buf[i])); i++);
    return buf;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Usage: %s FILE\nPrint word counts for FILE\n", argv[0]);
        return 1;
    }
    FILE *in = fopen(argv[1], "r");
    if (in)
    {
        int counter = 0;
        char buf[2048];
        buf[0] = '\0';

        WCHachTable ht;
        fscanf(in, "%*[^-0-9A-Za-z]");
        while (fscanf(in, "%[-0-9A-Za-z]", buf) != EOF)
        {
            ht.put(strToLower(buf));
            counter++;
            fscanf(in, "%*[^-0-9A-Za-z]");
        }
        fclose(in);

        int uniqWords = ht.printResult();
        printf("Total: %d words. Average frequency: %.3lf\n",
            counter, ((double) counter) / uniqWords);
        printf("Load factor: %.3lf\n", ht.getLoadFactor());
        printf("Average filling chains: %.3lf\n", ht.getAvrFillingTrees());
        printf("Maximum filling chains: %.3lf\n", ht.getMaxFillingTrees());
    }
    else
        printf("E: file %s not found\n", argv[1]);

    return 0;
}
