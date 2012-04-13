#pragma once
#include <stdexcept>
#include <iostream>
#include <string>
#include "../../hw02/hw02_task01/List/LinkedList.h"

class HasherIsNotSetException : public std::runtime_error
{
public:
    HasherIsNotSetException(std::string const& what_arg)
        : std::runtime_error(what_arg)
    {}
};

template <typename T>
class HashTable
{
public:

    class Hasher
    {
    public:
        virtual ~Hasher() {}
        virtual int operator ()(T const& item) = 0;
    };

    class DummyHasher : public Hasher
    {
    public:
        int operator ()(T const& item)
        { throw HasherIsNotSetException("Set a hash-functor using the method "
                                        "setHasher before using the table"); }
    };

    HashTable(size_t capacity = defaultCapacity);

    ~HashTable()
    { delete[] mTable; }

    void setHasher(Hasher *hasher)
    { mHasher = hasher; }

    bool hasherIsSet() const
    { return mHasher != &mDummyHasher; }

    void add(T const& item)
    { mTable[(*mHasher)(item)].insert(0, item); }

    void del(T const& item);
    T const& find(T const& item) const;
    void printStat(std::ostream &os = std::cout) const;

private:
    static const int defaultCapacity = 1000;
    static DummyHasher mDummyHasher;

    Hasher *mHasher;
    size_t mCapacity;
    LinkedList<T> *mTable;
};

template <typename T>
typename HashTable<T>::DummyHasher HashTable<T>::mDummyHasher = HashTable<T>::DummyHasher();


template <typename T>
HashTable<T>::HashTable(size_t capacity)
    : mHasher(&mDummyHasher)
    , mCapacity(capacity)
    , mTable(new LinkedList<T>[mCapacity])
{}

template <typename T>
T const& HashTable<T>::find(T const& item) const
{
    int hash = (*mHasher)(item);
    int index = mTable[hash].find(item);

    if (index == mTable->itemNotFound)
        return *(T *)0;
    return mTable[hash].at(index);
}

template <typename T>
void HashTable<T>::del(T const& item)
{
    int hash = (*mHasher)(item);
    int index = mTable[hash].find(item);

    mTable[hash].remove(index);
}

template <typename T>
void HashTable<T>::printStat(std::ostream &os) const
{
    int maxSizeOfChain = 0;
    int itemCounter = 0;
    int conflictCounter = 0;
    int busyCounter = 0;
    for (size_t i = 0; i < mCapacity; ++i)
    {
        int currSize = mTable[i].length();
        itemCounter += currSize;

        if (currSize > 0)
            busyCounter++;

        if (currSize > 1)
            conflictCounter++;

        if (currSize > maxSizeOfChain)
            maxSizeOfChain = currSize;
    }

    os << "number of items: " << itemCounter << std::endl;
    os << "load factor: " << (double)busyCounter / mCapacity << std::endl;
    os << "number of cells: " << mCapacity << std::endl;
    os << "number of busy cells: " << busyCounter << std::endl;
    os << "number of conflicts: " << conflictCounter << std::endl;
    os << "max size of chain: " << maxSizeOfChain << std::endl;
}

int hashTableTestExec(int argc, char **argv);
