#pragma once
#include "stack.h"

template <class T>
class SimpleStack : virtual public Stack<T>
{
public:
    SimpleStack(int capacity = stackCapacity);
    ~SimpleStack() { delete[] mArray; }

    void push(T value);
    T pop();

    T look() const;
    bool isEmpty() const { return mSize == 0; }
    int size() const { return mSize; }

private:
    int mCapacity;
    int mSize;
    T *mArray;

    static const int stackCapacity = 256;
};


template <class T>
SimpleStack<T>::SimpleStack(int capacity) :
    mCapacity(capacity),
    mSize(0),
    mArray(new T[capacity])
{}

template <class T>
T SimpleStack<T>::pop()
{
    if (mSize == 0)
        throw new StackUnderflowException();

    return mArray[--mSize];
}

template <class T>
T SimpleStack<T>::look() const
{
    if (mSize == 0)
        throw new StackUnderflowException();

    return mArray[mSize - 1];
}

template <class T>
void SimpleStack<T>::push(T value)
{
    if (mSize == mCapacity)
        throw new StackOverflowException();

    mArray[mSize++] = value;
}
