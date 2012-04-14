#pragma once
#include "List.h"

template <typename T>
class ArrayList : public List<T>
{
public:
    ArrayList(int initCapacity = defaultArrayListCapacity);
    ~ArrayList();

    void insert(size_t position, T item);
    void remove(size_t position);
    T & at(size_t position);

    int find(T item) const;
    size_t length() const { return mLength; }

private:
    void checkCapacity();

    static const int defaultArrayListCapacity = 2;
    static const int capacityMultiplier = 2;

    size_t mLength;
    size_t mCapacity;
    T *mArray;
};

template <typename T>
ArrayList<T>::ArrayList(int initCapacity) :
    mLength(0),
    mCapacity(initCapacity),
    mArray(new T[mCapacity])
{}

template <typename T>
ArrayList<T>::~ArrayList()
{
    delete[] mArray;
}

template <typename T>
void ArrayList<T>::checkCapacity()
{
    if (mLength != mCapacity)
        return;

    mCapacity *= capacityMultiplier;
    T *tmp = new T[mCapacity];

    for (size_t i = 0; i < mLength; i++)
    {
        tmp[i] = mArray[i];
    }

    delete[] mArray;
    mArray = tmp;
}

template <typename T>
void ArrayList<T>::insert(size_t position, T item)
{
    if (position > mLength)
        throw ListOutOfBoundsException();

    checkCapacity();

    for (size_t i = mLength; i > position; i--)
    {
        mArray[i] = mArray[i - 1];
    }
    mLength++;

    mArray[position] = item;
}

template <typename T>
void ArrayList<T>::remove(size_t position)
{
    if (position >= mLength)
        throw ListOutOfBoundsException();

    for (size_t i = position; i < mLength - 1; i++)
    {
        mArray[i] = mArray[i + 1];
    }
    mLength--;
}

template <typename T>
T & ArrayList<T>::at(size_t position)
{
    if (position >= mLength)
        throw ListOutOfBoundsException();
    return mArray[position];
}

template <typename T>
int ArrayList<T>::find(T item) const
{
    for (size_t i = 0; i < mLength; i++)
    {
        if (mArray[i] == item)
            return i;
    }
    return List<T>::itemNotFound;
}
