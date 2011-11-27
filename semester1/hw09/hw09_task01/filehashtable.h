#pragma once
#include <cstdio>

const int hashTableSizeP2 = 15;
const int maxLineLength = 1024;

struct ListNode
{
    int m_lineNo;
    ListNode *m_next;

    ListNode(int lineNo, ListNode *next) :
        m_lineNo(lineNo),
        m_next(next)
    {}
};

ListNode **createFileHashTable(FILE *f, int hashTableSizeP2);
bool inFileHashTable(const char *line, ListNode **table, int hashTableSizeP2, FILE *f);
void eraseTable(ListNode **table, int hashTableSizeP2);
