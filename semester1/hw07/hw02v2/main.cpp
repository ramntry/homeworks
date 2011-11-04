#include <cstdio>
#include "wordcounter.v2.h"
#include "wchashtable.h"

int main(void)
{
/*
    WordBST * tree = new WordBST;
    unsigned int i = 0;
    for (; i < sizeof(WordBST); i++)
        ((char *) tree)[i] = '*';
    tree->next = NULL;
    tree->top = 0;

    append(tree, "i");
    handle(tree, "am");
    handle(tree, "programming");
    handle(tree, "am");

    FILE *in = fopen("in", "r");
    int counter = 0;
    char buf[2048];
    buf[0] = '\0';
    if (in)
    {
        while (fscanf(in, "%s", buf) != EOF && (handle(tree, buf) != -1))
            counter++;
    }
    printWordBST(tree);
    printf("\n%d words readed from \"in\" (stop word is \"%s\")\n", counter, buf);
    delete tree;
    fclose(in);
*/
{
    char *a = new char [4096];
    for (int i = 0; i < 4096; i++)
        a[i] = '*';
    char **m = new char *[512];
    delete[] a;
    for (int i = 0; i < 512; i++)
    {
        m[i] = new char[128];
        for (int j = 0; j < 128; j++)
            m[i][j] = '*';
    }
    for (int i = 0; i < 512; i++)
    {
        delete[] m[i];
    }
    delete[] m;
}

    WCHachTable ht;

    FILE *in = fopen("in", "r");
    int counter = 0;
    char buf[2048];
    buf[0] = '\0';
    if (in)
        while (fscanf(in, "%s", buf) != EOF)
        {
            ht.put(buf);
            counter++;
        }
    fclose(in);

    ht.printResult();

    return 0;
}
