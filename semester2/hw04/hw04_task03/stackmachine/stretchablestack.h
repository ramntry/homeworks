#pragma once
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
}

template <class T>
StretchableStack<T>::StretchableStack(int capacity) :
    mHeadBlock(new StackBlock(capacity, NULL)),
    mCurrentBlock(mHeadBlock)
{}

template <class T>
StretchableStack<T>::~StretchableStack()
{
    StackBlock *current = mHeadBlock;
    while (current != NULL)
    {
        StackBlock *tmp = current->next;
        delete current;
        current = tmp;
    }
}

template <class T>
void StretchableStack<T>::push(T value)
{
    if (mCurrentBlock->length == mCurrentBlock->capacity)
    {
        if (mCurrentBlock->next == NULL)
            mCurrentBlock->next = new StackBlock(mCurrentBlock->capacity * capacityMultiplier, mCurrentBlock);

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
