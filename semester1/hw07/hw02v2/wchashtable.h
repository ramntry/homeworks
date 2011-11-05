#pragma once
#include "wordcounter.v2.h"
#include <cstdlib>

/**
 * Специализированная хеш-таблица для подсчета количества вхождений строк во входной поток.
 * Использует собственное распределение памяти, вывозы new минимизированы.
 * Модуль является клиентом модуля wordcounter.v2
 */

//const int hashTableSizeP2 = 3;  // DEBUG MODE. Release value is 13
const int hashTableSizeP2 = 13;
//const int sizeOfBlock = 4;      // DEBUG MODE. Release value is 128
const int sizeOfBlock = 128;

const int hashTableSize = 1 << hashTableSizeP2;

class WordBSTOverflowTooLengthStringException {};
class WordBSTOverflowTooManyWordsException {};

/**
 * Блок пула MemoryPool. Имеет фиксированный размер
 */
struct BlockPool
{
    BlockPool *prev;
    WordBST pool[sizeOfBlock];

    BlockPool() : prev(NULL) {}
};

/**
 * Линейный накопительный (только растущий) пул элементов фиксированного размера. Объемлющий пул - стандартная куча.
 * При недостатке емкости расширяется блоками равного размера, связанными в односвязный список (необходимо для
 * корректного удаления). Уничтожается весь пул одномоментно
 */
class MemoryPool
{
public:
    MemoryPool() : block(new BlockPool()), top(0) {}
    ~MemoryPool();

    WordBST *getWordBST();  // Основной метод-аллокатор

private:
    BlockPool *block;       // Текущий актуальный блок
    int top;                // ... и очередная свободная позиция в нем
};

class WCHachTable
{
public:
    WCHachTable();
    ~WCHachTable() { delete[] m_hashTable; }

    void put(const char *word);       // Подать строку на обработку
    int printResult();                // Распечатать гистограмму частот вхождений строк
    void printStat();

protected:
    unsigned int hashFunction(const char *word);

private:
    MemoryPool m_pool;           // Сама таблица (разрешающая коллизии методом цепочек) и объемлющий пул цепочек,
    WordBST **m_hashTable;       // ... представленных специализированными BS-деревьми WordBST
    unsigned int m_size;         // Текущее количество строк.
};
