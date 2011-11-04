#include <cstdio>
#include "wordcounter.v2.h"
#include "wchashtable.h"

int main(void)
{
    printf("Counting the frequency of occurrence of words in the file.\n%s",
           "Enter the file name: ");
    char filename[128];
    scanf("%s", filename);

    FILE *in = fopen(filename, "r");
    int counter = 0;
    char buf[2048];
    buf[0] = '\0';

    if (in)
    {
        WCHachTable ht;
        while (fscanf(in, "%s", buf) != EOF)
        {
            ht.put(buf);
            counter++;
        }
        fclose(in);

        ht.printResult();
        printf("Load factor: %.3lf\n", ht.getLoadFactor());
        printf("Average filling chains: %.3lf\n", ht.getAvrFillingTrees());
        printf("Maximum filling chains: %.3lf\n", ht.getMaxFillingTrees());
    }
    else
        printf("E: file \"%s\" not found\n", filename);

    return 0;
}
