#include "wchashtable.h"
#include <cstring>
#include <cstdio>

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

/**
 * Хеширование методом умножений. Кормен и Лейзерсон, "Алгоритмы. Построение и анализ", 1-ое изд., перевод МЦНМО,
 * стр. 222.
 */
unsigned int WCHachTable::hashFunction(const char *word)
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

void WCHachTable::put(const char *word)
{
    unsigned int hash = hashFunction(word);
    if (m_hashTable[hash])
    {
        int counter = handle(m_hashTable[hash], word);
        if (counter == 0)  // было добавлено новое слово
            m_size++;
        else if (counter == -1)
            throw WordBSTOverflowTooManyWordsException();
    }
    else
    {
        m_hashTable[hash] = m_pool.getWordBST();
        int writed = append(m_hashTable[hash], word);        //  Инициализация значением (деревья не используют
        if (word[writed])                                    //  ... охранников)
            throw WordBSTOverflowTooLengthStringException();
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

void WCHachTable::printStat()
{
    double treesNumber = 0.0;
    double maximum = 0.0;
    double accumulator = 0.0;
    double current = 0.0;
    for (int i = 0; i < hashTableSize; i++)
        if (m_hashTable[i])
        {
            current = (double) (m_hashTable[i]->top) / capacityWordBST;
            maximum = current > maximum ? current : maximum;
            accumulator += current;
            treesNumber++;
        }
    double avrWordSize = accumulator * usefulCapacityWBST / m_size - averageLoss;
    double loadFactor  = (double) m_size / hashTableSize;
    double avrFilling  = accumulator / treesNumber;
    printf
    (
        "Average word size about: %.3lf\n"
        "Load factor: %.3lf\n"
        "Average filling chains : %.3lf\n"
        "Maximum filling chains : %.3lf\n"
        ,avrWordSize ,loadFactor ,avrFilling ,maximum
    );
}
