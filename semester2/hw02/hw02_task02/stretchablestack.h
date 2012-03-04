#pragma once
#include <iostream>
#include "stack.h"

template <class T>
class StretchableStack : public Stack<T>
{
public:
    StretchableStack(int capacity = defaultStackCapacity);
    ~StretchableStack();

    void push(T value);
    T pop();

    T look() const;
    bool isEmpty() const;
    int size() const;

private:
    struct StackBlock
    {
        StackBlock(int newCapacity, StackBlock *prevBl);
        ~StackBlock();

        T *array;

        int length;
        int capacity;
        StackBlock *next;
        StackBlock *prev;
    };

    StackBlock *mHeadBlock;
    StackBlock *mCurrentBlock;

    static const int defaultStackCapacity = 32;
    static const int capacityMultiplier = 2;
};


template <class T>
StretchableStack<T>::StackBlock::StackBlock(int newCapacity, StackBlock *prevBl) :
    array(new T[newCapacity]),
    length(0),
    capacity(newCapacity),
    next(NULL),
    prev(prevBl)
{}

template <class T>
StretchableStack<T>::StackBlock::~StackBlock()
{
    delete[] array;
    delete next;
}

template <class T>
StretchableStack<T>::StretchableStack(int capacity) :
    mHeadBlock(new StackBlock(capacity, NULL)),
    mCurrentBlock(mHeadBlock)
{}

template <class T>
StretchableStack<T>::~StretchableStack()
{
    delete mHeadBlock;
}

template <class T>
void StretchableStack<T>::push(T value)
{
    if (mCurrentBlock->length == mCurrentBlock->capacity)
    {
        if (mCurrentBlock->next == NULL)
        {
            mCurrentBlock->next = new StackBlock(mCurrentBlock->capacity * capacityMultiplier, mCurrentBlock);
            std::cout << "StackBlock of " << mCurrentBlock->capacity << " elements is created" << std::endl;
        } else
            std::cout << "StackBlock of " << mCurrentBlock->capacity << " elements is reuse" << std::endl;

        mCurrentBlock = mCurrentBlock->next;
    }

    mCurrentBlock->array[mCurrentBlock->length++] = value;
}

template <class T>
T StretchableStack<T>::pop()
{
    if (mCurrentBlock->length == 0)
        mCurrentBlock = mCurrentBlock->prev;

    if (mCurrentBlock == NULL)
        throw new StackUnderflowException();

    return mCurrentBlock->array[--mCurrentBlock->length];
}

template <class T>
T StretchableStack<T>::look() const
{
    StackBlock *top = mCurrentBlock;
    if (top->length == 0)
        top = top->prev;

    if (top == NULL)
        throw new StackUnderflowException();

    return top->array[top->length - 1];
}

template <class T>
int StretchableStack<T>::size() const
{
    int accumulator = 0;
    StackBlock *current = mHeadBlock;
    while (current != NULL)
    {
        accumulator += current->length;
        current = current->next;
    }
    return accumulator;
}

template <class T>
bool StretchableStack<T>::isEmpty() const
{
    return mCurrentBlock->length == 0 && mCurrentBlock->prev == NULL;
}
