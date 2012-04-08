#pragma once
/*
#include <stdexcept>
#include <iostream>
#include <string>

class HasherIsNotSetException : public std::runtime_error
{
public:
    HasherIsNotSetException(std::string const& what_arg)
        : std::runtime_error(what_arg)
    {}
};

template <typename T>
class Hasher
{
public:
    ~Hasher() {}
    virtual int operator ()(T const& item) throw (HasherIsNotSetException) = 0;
};

template <typename T>
class DummyHasher : public Hasher<T>
{
public:
    int operator ()(T const& item) throw (HasherIsNotSetException)
    { throw HasherIsNotSetException("Set a hash-functor using the method "
                                    "setHasher before using the table"); }
};

template <typename T>
class HashTable
{
    struct ChainLink
    {
        ChainLink(T const& _item, ChainLink *_next)
            : item(_item)
            , next(_next)
        {}

        T item;
        ChainLink *next;
    };

public:
    HashTable();
    ~HashTable();

    void setHasher(Hasher<T> &hasher);

    bool hasherIsSet() const
    { return &mHasher != mDummyHasher; }

    void add(T const& item);
    void del(T const& item);

    T & find(T const& item);
    T const& find(T const& item) const;

    void printStat(std::ostream &os = std::cout) const;

private:
    ChainLink *findLink(T const& item);

    DummyHasher<T> *mDummyHasher;
    Hasher<T> &mHasher;
    size_t mCapacity;
    ChainLink **mTable;
    int mLastCalculatedHash;

    static const int defaultCapacity = 1000;
};

template <typename T>
HashTable<T>::HashTable()
    : mDummyHasher(new DummyHasher<T>)
    , mHasher(*mDummyHasher)
    , mCapacity(defaultCapacity)
    , mTable(new ChainLink*[mCapacity])
    , mLastCalculatedHash(-1)
{}

template <typename T>
HashTable<T>::~HashTable()
{
    delete mDummyHasher;
    delete[] mTable;
}

template <typename T>
void HashTable<T>::setHasher(Hasher<T> &hasher)
{
    mHasher = hasher;
}

template <typename T>
HashTable::ChainLink *HashTable<T>::findLink(T const& item)
{
    mLastCalculatedHash = mHasher(item);
    ChainLink current = mTable[mLastCalculatedHash];

    while (current.next && current.next->value != item)
        current = current.next;

    return current;
}

template <typename T>
T &HashTable<T>::find(T const& item)
{
    ChainLinkBase *link = findLink(item);
    if (link->next)
        return link->next->value;
    return *(T *)0;
}

template <typename T>
void HashTable<T>::add(T const& item)
{
    if (!findLink(item))
        return;

    new ChainLink(mTable[mLastCalculatedHash], item);
}

template <typename T>
void HashTable<T>::del(T const& item)
{
    ChainLink *link = findLink(item);
    if (!link)
        return;

}
*/
