#pragma once
#include "List.h"

template <typename T>
class ArrayList : public List<T>
{
public:
    ArrayList(int initCapacity = defaultArrayListCapacity);
    ~ArrayList();

    void insert(int position, T item);
    void remove(int position);
    T & at(int position);

    int find(T item) const;
    int length() const { return mLength; }

private:
    void checkCapacity();

    static const int defaultArrayListCapacity = 256;
    static const int capacityMultiplier = 2;

    int mLength;
    int mCapacity;
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

    for (int i = 0; i < mLength; i++)
    {
        tmp[i] = mArray[i];
    }

    delete[] mArray;
    mArray = tmp;
}

template <typename T>
void ArrayList<T>::insert(int position, T item)
{
    if (position > mLength)
        throw ListOutOfBoundsException();

    checkCapacity();

    for (int i = mLength; i > position; i--)
    {
        mArray[i] = mArray[i - 1];
    }
    mLength++;

    mArray[position] = item;
}

template <typename T>
void ArrayList<T>::remove(int position)
{
    if (position >= mLength)
        throw ListOutOfBoundsException();

    for (int i = position; i < mLength - 1; i++)
    {
        mArray[i] = mArray[i + 1];
    }
    mLength--;
}

template <typename T>
T & ArrayList<T>::at(int position)
{
    if (position >= mLength)
        throw ListOutOfBoundsException();
    return mArray[position];
}

template <typename T>
int ArrayList<T>::find(T item) const
{
    for (int i = 0; i < mLength; i++)
    {
        if (mArray[i] == item)
            return i;
    }
    return itemNotFound;
}
