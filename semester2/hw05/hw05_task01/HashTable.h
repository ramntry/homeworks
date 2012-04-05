#pragma once
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
    DummyHasher<T> *mDummyHasher;
    Hasher<T> &mHasher;
    T *mTable;

    static const int defaultCapacity = 1000;
};

template <typename T>
HashTable<T>::HashTable()
    : mDummyHasher(new DummyHasher<T>)
    , mHasher(*mDummyHasher)
    , mTable(new T[defaultCapacity])
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
