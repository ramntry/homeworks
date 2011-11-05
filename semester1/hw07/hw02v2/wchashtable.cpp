#include "wchashtable.h"
#include <cstring>

MemoryPool::~MemoryPool()
{
    while (block->prev)
    {
        BlockPool *tmp = block;
        block = block->prev;
        delete tmp;
    }
    delete block;
}

WordBST *MemoryPool::getWordBST()
{
    if (top == sizeOfBlock)       // При переполнении текущего актуального блока
    {
        BlockPool *tmp = block;
        block = new BlockPool();  // ... заводим и инициализируем очередной
        block->prev = tmp;
        top = 0;
    }
    WordBST *wbst = block->pool + top;
    top++;
    wbst->next = NULL;            // Деревья не имеют собственных конструкторов. Инициализируем вручную.
    wbst->top = 0;
    return wbst;
}

WCHachTable::WCHachTable() :
    m_pool(),
    m_hashTable(new WordBST *[hashTableSize]),
    m_size(0)
{
    for (int i = 0; i < hashTableSize; i++)
        m_hashTable[i] = NULL;                 //  Изначально хеш-таблица пуста. В связи с большим размером деревьев
}                                              //  ... поиска они выделяются по необходимости

WCHachTable::~WCHachTable()
{
    delete[] m_hashTable;
}

/**
 * Хеширование методом умножений. Кормен и Лейзерсон, "Алгоритмы. Построение и анализ", 1-ое изд., перевод МЦНМО,
 * стр. 222.
 */
/*
unsigned int WCHachTable::hashFunction(const char *word)
{
    unsigned int A2w = 2654435769;  // int(((sqrt(5) - 1) / 2) * 2**32)
    unsigned int hash = 0;
    unsigned int accumulator = 0;
    for (int i = 0; word[i]; i++)
    {
        accumulator <<= 8;
        accumulator += (unsigned char) word[i];
        if (i % 4 == 3)
        {
            hash ^= (accumulator * A2w) >> (32 - hashTableSizeP2);
            accumulator = 0;
        }
    }
    hash ^= (accumulator * A2w) >> (32 - hashTableSizeP2);

    return hash;
}
unsigned int WCHachTable::hashFunction(const char *word)
{
    unsigned long long A2w = 11400714819323199488ULL;  // int((sqrt(5) - 1) / 2 * 2**64)
    unsigned long long hash = 0;
    int len = strlen(word);
    int i = 0;
    for (; i < len - 7; i += 8)
        hash ^= *((unsigned long long *) (word + i)) * A2w;
    word += i;
    len -= i;
    if (len > 3)
    {
        hash ^= *((unsigned int*) word) * A2w;
        word += 4;
        len -= 4;
    }
    if (len > 1)
    {
        hash ^= *((unsigned short *) word) * A2w;
        word += 2;
        len -= 2;
    }
    if (len > 0)
        hash ^= *((unsigned char *) word) * A2w;

    hash >>= (64 - hashTableSizeP2);
    return hash;
}
*/

unsigned int WCHachTable::hashFunction(const char *word)
{
    unsigned int A2w = 2654435769;  // int(((sqrt(5) - 1) / 2) * 2**32)
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

void WCHachTable::put(const char *word)
{
    unsigned int hash = hashFunction(word);
    if (m_hashTable[hash])
    {
        int counter = handle(m_hashTable[hash], word);
        if (counter == 0)  // было добавлено новое слово
            m_size++;
        else if (counter == -1)
            throw WordBSTOverflowExceptionToManyWords();
    }
    else
    {
        m_hashTable[hash] = m_pool.getWordBST();
        int writed = append(m_hashTable[hash], word);        //  Инициализация значением (деревья не используют
        if (word[writed])                                    //  ... охранников)
            throw WordBSTOverflowExceptionToLengthString();
        m_size++;
    }
}

int WCHachTable::printResult()
{
    int currentNumber = 1;
    for (int i = 0; i < hashTableSize; i++)
        if (m_hashTable[i])
            currentNumber = printWordBST(m_hashTable[i], currentNumber);
    return currentNumber - 1;
}

double WCHachTable::getAvrFillingTrees()
{
    double treesNumber = 0.0;
    double accumulator = 0.0;
    for (int i = 0; i < hashTableSize; i++)
        if (m_hashTable[i])
        {
            accumulator += (double) (m_hashTable[i]->top) / capacityWordBST;
            treesNumber++;
        }
    return accumulator / treesNumber;
}

double WCHachTable::getMaxFillingTrees()
{
    double current = 0.0;
    double maximum = 0.0;
    for (int i = 0; i < hashTableSize; i++)
        if (m_hashTable[i])
        {
            current = (double) (m_hashTable[i]->top) / capacityWordBST;
            maximum = current > maximum ? current : maximum;
        }
    return maximum;
}
