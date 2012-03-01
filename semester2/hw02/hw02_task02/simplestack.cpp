#include "simplestack.h"

SimpleStack::SimpleStack(int capacity) :
    mCapacity(capacity),
    mSize(0),
    mArray(new StackElement[capacity])
{}

StackElement SimpleStack::pop()
{
    if (mSize == 0)
        throw new StackUnderflowException();

    return mArray[--mSize];
}

void SimpleStack::push(StackElement value)
{
    if (mSize == mCapacity)
        throw new StackOverflowException();

    mArray[mSize++] = value;
}
