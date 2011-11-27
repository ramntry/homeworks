#include <cstdio>
#include <cstring>
#include "filehashtable.h"

unsigned int hashFunction(const char *word, int hashTableSizeP2)
{
    unsigned int A2w = 2654435761;  // ближайшее простое к int(((sqrt(5) - 1) / 2) * 2**32)
    unsigned int hash = 0;
    int len = strlen(word);
    int i = 0;
    for (; i < len - 3; i += 4)
        hash ^= *((unsigned int *) (word + i)) * A2w;
    word += i;
    len -= i;
    if (len > 1)
    {
        hash ^= *((unsigned short *) word) * A2w;
        word += 2;
        len -= 2;
    }
    if (len > 0)
        hash ^= *((unsigned char *) word) * A2w;

    hash >>= (32 - hashTableSizeP2);
    return hash;
}

bool inFileHashTable(const char *line, ListNode **table, int hashTableSizeP2, FILE *f)
{
    unsigned int hash = hashFunction(line, hashTableSizeP2);
    ListNode *current = table[hash];
    while (current != NULL)
    {
        char buf[maxLineLength];
        int oldPos = ftell(f);
        fseek(f, current->m_lineNo, SEEK_SET);
        fgets(buf, maxLineLength, f);
        fseek(f, oldPos, SEEK_SET);

        if (strcmp(buf, line) == 0)
            return true;

        current = current->m_next;
    }
    return false;
}

ListNode **createFileHashTable(FILE *f, int hashTableSizeP2)
{
    int size = 1 << hashTableSizeP2;
    ListNode **table = new ListNode*[size];
    for (int i = 0; i < size; i++)
        table[i] = NULL;

    char buf[maxLineLength];
    int lineNo = ftell(f);
    while (fgets(buf, maxLineLength, f) != (char *) NULL)
    {
        unsigned int hash = hashFunction(buf, hashTableSizeP2);
        table[hash] = new ListNode(lineNo, table[hash]);
        lineNo = ftell(f);
    }
    return table;
}

void eraseTable(ListNode **table, int hashTableSizeP2)
{
    int size = 1 << hashTableSizeP2;
    for (int i = 0; i < size; i++)
    {
        ListNode *current = table[i];
        while (current != NULL)
        {
            ListNode *tmp = current->m_next;
            delete current;
            current = tmp;
        }
    }
    delete[] table;
}
