#include "filehashtable.h"
#include <cstdio>
#include <cstring>

class FileNotFoundException {};

FILE *fileOpen(const char *promt, const char *mode)
{
    char buf[540];
    printf("%s", promt);
    scanf("%s", buf);
    FILE *f = fopen(buf, mode);
    if (!f)
        throw FileNotFoundException();
    return f;
}

int main() try
{
    printf("Writing to a file destination row from the second input file\n"
           "if there is a line in the first input file.\n\n");
    FILE *fst = fileOpen("Fist source file name: ", "r");
    FILE *snd = fileOpen("Second source file name: ", "r");
    FILE *dst = fileOpen("Destination file name: ", "w");

    ListNode **table = createFileHashTable(fst, hashTableSizeP2);

    char buf[maxLineLength];
    while (fgets(buf, maxLineLength, snd) != (char *) NULL)
    {
        if (inFileHashTable(buf, table, hashTableSizeP2, fst))
            fprintf(dst, "%s", buf);
    }

    fclose(fst);
    fclose(snd);
    fclose(dst);
    eraseTable(table, hashTableSizeP2);
    return 0;
}

    catch (FileNotFoundException)
    {
        fprintf(stderr, "File not found\n");
        return 1;
    }
