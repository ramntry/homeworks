#include "wordcounter.v2.h"
#include <cstring>
#include <cstdio>

inline
Direction strCmp(const char *fst, const char *snd)
{
        // Почему здесь медленнее работает стандартная strcmp - не ясно
        int i = 0;
        for (; fst[i] && fst[i] == snd[i]; i++);
        if (fst[i] < snd[i])
                return toLeftChild;       // word меньше. Нам в левую ветвь.
        if (fst[i] > snd[i])
                return toRightChild;      // word больше. Нам в правую ветвь.
        return stopSearch;
}
inline int calcFreeSpace(unsigned int top)
{
    return capacityWordBST * sizeof(int) - addFieldsSize - 1 - top * sizeof(int);
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
    topNode->childs[toLeftChild] = 0;             // ... и зануление индексов сыновей
    topNode->childs[toRightChild] = 0;

    int writed = 0;
    while (word[writed] && writed < freeSpace)    // Копирование строки с контролем выхода за границы дерева
    {
       topNode->word[writed] = word[writed];
       writed++;
    }
    topNode->word[writed] = '\0';                 // Гарантированное терминирование строки нулем.
    tree->top += calcAllocatedSpace(writed);      // Безопасное смещение top

    return writed;
}

int handle(WordBST *tree, const char *word)
{
    unsigned int index = 0;
    for (;;)
    {
        Direction resCmp = strCmp(word, tree->nodes[index].word);
        if (resCmp == stopSearch)  // если строки равны
            return (tree->counters[index])++;

        if (tree->nodes[index].childs[resCmp])         // Если есть нужный сын,
            index = tree->nodes[index].childs[resCmp]; // ... идем туда
        else
        {
            unsigned int newNodeIndex = tree->top;             // Запомним индекс добавляемого элемента
            int writed = append(tree, word);                   // Попытаемся его разместить
            if (word[writed])
                return -1;                                     // Если удачно -
            tree->nodes[index].childs[resCmp] = newNodeIndex;  // ... прописываемся у родителя
            return 0;
        }
    }
}

int printWordBST(WordBST *tree, int enumStarts, unsigned int startsWith)
{
    static int currentNum = enumStarts;

    if (tree->nodes[startsWith].childs[toLeftChild])
        printWordBST(tree, currentNum, tree->nodes[startsWith].childs[toLeftChild]);

    if (currentNum != -1)
        printf("%5d:", currentNum++);
    printf("%20s - %d\n", tree->nodes[startsWith].word, tree->counters[startsWith]);

    if (tree->nodes[startsWith].childs[toRightChild])
        printWordBST(tree, currentNum, tree->nodes[startsWith].childs[toRightChild]);

    return currentNum;
}
