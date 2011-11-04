#include "wordcounter.v2.h"
#include <cstring>

inline int strCmp(const char * fst, const char * snd)  // Почему здесь перестает работать стандартная strcmp - не ясно
{
        int i = 0;
        for (; fst[i] && fst[i] == snd[i]; i++);
        if (fst[i] > snd[i])
                return 1;
        if (fst[i] < snd[i])
                return -1;
        return 0;
}

inline int calcFreeSpace(unsigned int top)
{
    return SIZE * sizeof(int) - addFieldsSize - 1 - top * sizeof(int);
}

inline int calcAllocatedSpace(int writed)
{
    return (2 * sizeof(int) + addFieldsSize + writed) / sizeof(int);
}

int append(WordBST *tree, const char *word)
{
    int freeSpace = calcFreeSpace(tree->top);
    if (freeSpace <= 0)
        return 0;
    tree->counters[tree->top] = 1;                // инициализация счетчика
    WordNode *topNode = tree->nodes + tree->top;
    topNode->leftChild = 0;                       // ... и зануление индексов сыновей
    topNode->rightChild = 0;

    int writed = 0;
    while (word[writed] && writed < freeSpace)    // Копирование строки с контролем выхода за границы дерева
    {
       (&(topNode->firstLetter))[writed] = word[writed];
        writed++;
    }
    (&(topNode->firstLetter))[writed] = '\0';     // Гарантированное терминирование строки нулем.
    tree->top += calcAllocatedSpace(writed);      // Безопасное смещение top

    return writed;
}

int handle(WordBST *tree, const char *word)
{
    unsigned int index = 0;
    for (;;)
    {
        switch (strCmp(word, &(tree->nodes[index].firstLetter)))
        {
        case 0:
            return (tree->counters[index])++;
        case -1:
            if (tree->nodes[index].leftChild)
            {
                index = tree->nodes[index].leftChild;
                break;
            }
            else
            {
                unsigned int newNodeIndex = tree->top;
                int writed = append(tree, word);
                if (word[writed])
                    return -1;
                tree->nodes[index].leftChild = newNodeIndex;
                return 0;
            }
        case 1:
            if (tree->nodes[index].rightChild)
            {
                index = tree->nodes[index].rightChild;
                break;
            }
            else
            {
                unsigned int newNodeIndex = tree->top;
                int writed = append(tree, word);
                if (word[writed])
                    return -1;
                tree->nodes[index].rightChild = newNodeIndex;
                return 0;
            }
        }
    }
}
