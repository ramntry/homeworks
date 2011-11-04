#pragma once
#include "wordcounter.v2.h"
#include <cstdlib>

/**
 * Специализированная хеш-таблица для подсчета количества вхождений строк во входной поток.
 * Использует собственное распределение памяти, вывозы new минимизированы.
 * Модуль является клиентом модуля wordcounter.v2
 */

const int hashTableSizeP2 = 3;  // 11
const int sizeOfBlock = 4;      // 256

const int hashTableSize = 1 << hashTableSizeP2;

class WordBSTOverflowExceptionToLengthString {};
class WordBSTOverflowExceptionToManyWords {};

struct BlockPool
{
    BlockPool *prev;
    WordBST pool[sizeOfBlock];

    BlockPool() : prev(NULL) {}
};

class MemoryPool
{
public:
    MemoryPool() : block(new BlockPool()), top(0) {}
    ~MemoryPool();

    WordBST *getWordBST();

private:
    BlockPool *block;
    int top;

};

class WCHachTable
{
public:
    WCHachTable();
    ~WCHachTable();
    void put(const char *word);
    void printResult();

protected:
    unsigned int hashFunction(const char *word);

private:
    MemoryPool m_pool;
    WordBST **m_hashTable;
};
