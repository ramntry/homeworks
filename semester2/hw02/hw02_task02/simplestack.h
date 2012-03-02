#pragma once
#include "stack.h"

class SimpleStack : public Stack
{
public:
    SimpleStack(int capacity = stackCapacity);
    ~SimpleStack() { delete[] mArray; }

    void push(StackElement value);
    StackElement pop();

    StackElement look() const;
    bool isEmpty() const { return mSize == 0; }
    int size() const { return mSize; }

private:
    int mCapacity;
    int mSize;
    StackElement *mArray;

    static const int stackCapacity = 256;
};
