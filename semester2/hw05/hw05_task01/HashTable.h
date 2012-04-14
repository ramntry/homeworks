#pragma once
#include <stdexcept>
#include <iostream>
#include <string>
#include "../../hw02/hw02_task01/List/ArrayList.h"

/**
 * Исключение, выбрасываемое при попытке использовать хеш-таблицу с неустановленной хеш-функцией
 */
class HasherIsNotSetException : public std::runtime_error
{
public:
    HasherIsNotSetException(std::string const& what_arg)
        : std::runtime_error(what_arg)
    {}
};

/**
 * Шаблонный класс, реализующий хеш-таблицу, использующую пользовательскую хеш-функцию и позволяющую менять
 * последнюю во время выполнения с полным перехешированием таблицы. Также перехеширование производится при достижении
 * load factor'ом некоторого критического значения (criticalLoadFactor).
 * @see criticalLoadFactor
 */
template <typename T>
class HashTable
{
public:

    /**
     * Перед использованием созданного экземпляра класса HashTable необходимо установить пользовательскую
     * хеш-функцию, реализующую интерфейс функтора Hasher (доступ к нему осуществляется в форме HashTable<T>::Hasher)
     */
    class Hasher
    {
    public:
        virtual ~Hasher() {}
        virtual int operator ()(T const& item) = 0;
    };

    /**
     * Хеш-функтор-заглушка, бросает исключение HasherIsNotSetException при попытке использования
     */
    class DummyHasher : public Hasher
    {
    public:
        int operator ()(T const& item)
        { throw HasherIsNotSetException("Set a hash-functor using the method "
                                        "setHasher before using the table"); }
    };

    /**
     * @param capacity Емкость (число ячеек) таблицы в начальный момент времени работы. Имеет значение по умолчанию
     */
    HashTable(size_t capacity = defaultCapacity);

    ~HashTable()
    { delete[] mTable; }

    /**
     * Метод, используемый для первичной установки (а также замены с перехешированием) пользовательской хеш-функции.
     * @param hasher Указатель на экземпляр реализации интерфейса HashTable<T>::Hasher.
     * @warning Класс не освобождает память по переданному в метод setHasher указателю.
     */
    void setHasher(Hasher *hasher);

    /**
     * @return true, если пользовательская хеш-функция установлена, false в противном случае
     */
    bool hasherIsSet() const
    { return mHasher != &mDummyHasher; }

    /**
     * @return Текущее число элементов в хеш-таблице
     */
    size_t size()
    { return mSize; }

      /** Добавление элемента в хеш-таблицу (элемент копируется) */
    void add(T const& item);
    /**
     * Удаление элемента из хеш-таблицы (по значению). При попытке удаления несуществующего элемента не выполняется
     * никаких действий.
     * @return true, если элемент присутстовал в таблице и был удален, false, если элемента не было в таблице
     */
    bool del(T const& item);

    /**
     * Поиск элемента в таблице. Сравнение на равенство выполняется с помощью operator =
     * @return Константая ссылка на найденный объект, либо ссылка на нулевой адрес памяти в противном случае.
     * @return Проверка на наличие элемента в таблице может осуществляться следующим образом: &t.find(item) != 0
     */
    T const& find(T const& item) const;

    /**
     * Печать в переданный поток (по умолчанию в стандартный поток вывода) статистики по хеш-таблице в формате
     * <свойство>: значение. В текущей реализации отображает следующие свойства:
     */
    void printStat(std::ostream &os = std::cout) const;

private:
    static const int defaultCapacity = 256;       /**< Значение начальной емкости таблицы по умолчанию */
    static const double desiredLoadFactor = 0.25; /**< Желаемый load factor. Максимальное актуальное значение
                                                       после любого перехеширования (как в связи с заменой хеш-
                                                       функции, так и в связи с переполенинем таблицы) */
    static const double criticalLoadFactor = 0.5; /**< Критический load factor. Максимальное актуальное значение
                                                       в любой момент времени. Определяет коэффициент увеличения
                                                       емкости таблицы при перехешировании при переполнении как
                                                       criticalLoadFactor / desiredLoadFactor */

    static DummyHasher mDummyHasher; /**< Разделяемый всеми экземплярами класса HashTable хеш-функтор по умолчанию
                                          (бросающий исключение HasherIsNotSetException при попытке использования */

    /**
     * Перехеширование таблицы. Создает новую таблицу емкости newCapacity и переписывает в нее все значения с пере-
     * хешированием относительно текущей хеш-функции
     */
    void rehash(size_t newCapacity);

    Hasher *mHasher;      /**< Текущий хеш-функтор */
    size_t mCapacity;     /**< Текущая емкость (число ячеек) таблицы */
    size_t mSize;         /**< Число элементов в таблице */
    ArrayList<T> *mTable;
};

template <typename T>
typename HashTable<T>::DummyHasher HashTable<T>::mDummyHasher = HashTable<T>::DummyHasher();

/**
 * В текущей реализации для разрешения конфликтов используется метод цепочек. В качестве цепочек используются
 * экземпляры класса ArrayList (интерфейс List) - список на динамическом (расширяющимся при переполнении) массиве.
 * @see ArrayList
 */
/* Согласно интерфейсу List, оперирование элементами цепочки осуществляется по целочисленному индексу, что в случае
 * реализации ArrayList выполняется эффективно.
 */

template <typename T>
HashTable<T>::HashTable(size_t capacity)
    : mHasher(&mDummyHasher)
    , mCapacity(capacity)
    , mSize(0)
    , mTable(new ArrayList<T>[mCapacity])
{}

// поиск
template <typename T>
T const& HashTable<T>::find(T const& item) const
{
    int hash = (*mHasher)(item) % mCapacity;
    int index = mTable[hash].find(item);

    if (index == mTable->itemNotFound)
        return *(T *)0;            // В качестве значения "не найден" используется ссылка на нулевой адрес памяти
    return mTable[hash].at(index);
}

// удаление
template <typename T>
bool HashTable<T>::del(T const& item)
{
    int hash = (*mHasher)(item) % mCapacity;
    int index = mTable[hash].find(item);

    if (index != List<T>::itemNotFound)
    {
        mTable[hash].remove(index);
        --mSize;
        return true;
    }
    return false;
}

// перехеширование
template <typename T>
void HashTable<T>::rehash(size_t newCapacity)
{
    ArrayList<T> *tmp = new ArrayList<T>[newCapacity];

    for (size_t i = 0; i < mCapacity; ++i)
        for (size_t j = 0; j < mTable[i].length(); ++j)
        {
            int hash = (*mHasher)(mTable[i].at(j)) % newCapacity;
            tmp[hash].insert(tmp[hash].length(), mTable[i].at(j)); // Вставка нового элемента осуществляется в конец
        }                                                          // списка из соображений эффективности

    mCapacity = newCapacity;
    delete[] mTable;
    mTable = tmp;
}

// установка хешера
template <typename T>
void HashTable<T>::setHasher(Hasher *hasher)
{
    mHasher = hasher;

    // пустую таблицу не перехешируем
    if (mSize == 0)
        return;

    // раз уж все равно перехешировать, заведем новую таблицу емкостью, удовлетворяющей desiredLoadFactor
    size_t newCapacity = std::max((double)mCapacity, mSize / desiredLoadFactor);

    rehash(newCapacity);
}

// добавление
template <typename T>
void HashTable<T>::add(T const& item)
{
    // перехешируем в случае превышения criticalLoadFactor. Емкость приводим к соответствию desiredLoadFactor
    if ((double)(mSize + 1) / mCapacity > criticalLoadFactor)
        rehash((mSize + 1) / desiredLoadFactor);

    // mCapacity актуален (меняется в rehash(size_t newCapacity))
    int hash = (*mHasher)(item) % mCapacity;
    int index = mTable[hash].find(item);

    // уже существующий элемент не добавляем
    if (index == List<T>::itemNotFound)
    {
        mTable[hash].insert(mTable[hash].length(), item);
        ++mSize;
    }
}

// печать статистики
template <typename T>
void HashTable<T>::printStat(std::ostream &os) const
{
    int maxSizeOfChain = 0;
    int busyCounter = 0;
    for (size_t i = 0; i < mCapacity; ++i)
    {
        int currSize = mTable[i].length();

        if (currSize > 0)
            busyCounter++;

        if (currSize > maxSizeOfChain)
            maxSizeOfChain = currSize;
    }

    os << "number of items: "      << mSize                    << std::endl; /// Число элементов в таблице
    os << "load factor: "          << (double)mSize/ mCapacity << std::endl; /// Отношение числа ячеек к числу элементов
    os << "number of cells: "      << mCapacity                << std::endl; /// Общее число ячеек в таблице
    os << "number of busy cells: " << busyCounter              << std::endl; /// Число ячеек с непустыми цепочками
    os << "number of conflicts: "  << mSize - busyCounter      << std::endl; /// Число ячеек с цепочками размером > 1
    os << "max size of chain: "    << maxSizeOfChain           << std::endl; /// Длина наибольшей цепочки
}

/**
 * Unit-тест класса на базе библиотеки QTestLIb
 */
int hashTableTestExec(int argc, char **argv);
