#include "wchashtable.h"
#include <cstring>
#include <cstdio>     // DELETE IT

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
    if (top == sizeOfBlock)
    {
        BlockPool *tmp = block;
        block = new BlockPool();
        block->prev = tmp;
        top = 0;
    }
    WordBST *wbst = block->pool + top;
    top++;
    wbst->next = NULL;
    wbst->top = 0;
    return wbst;
}

WCHachTable::WCHachTable() :
    m_pool(),
    m_hashTable(new WordBST *[hashTableSize])
{
    for (int i = 0; i < hashTableSize; i++)
        m_hashTable[i] = NULL;
}

WCHachTable::~WCHachTable()
{
    delete[] m_hashTable;
}

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

void WCHachTable::put(const char *word)
{
    unsigned int hash = hashFunction(word);
    if (m_hashTable[hash])
    {
        int counter = handle(m_hashTable[hash], word);
        if (counter == -1)
            throw WordBSTOverflowExceptionToManyWords();
    }
    else
    {
        m_hashTable[hash] = m_pool.getWordBST();
        int writed = append(m_hashTable[hash], word);
        if (word[writed])
            throw WordBSTOverflowExceptionToLengthString();
    }
}

void WCHachTable::printResult()
{
    int currentNumber = 1;
    for (int i = 0; i < hashTableSize; i++)
        if (m_hashTable[i])
        {
            currentNumber += printWordBST(m_hashTable[i], currentNumber);
            putchar('\n');
        }
}
