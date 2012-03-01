#pragma once
#include "stack.h"


class StretchableStack : public Stack
{
public:
    StretchableStack(int capacity = defaultStackCapacity);
    ~StretchableStack();

    void push(StackElement value);
    StackElement pop();

    bool isEmpty() const;
    int size() const;

private:
    struct StackBlock
    {
        StackBlock(int newCapacity, StackBlock *prevBl);
        ~StackBlock();

        StackElement *array;

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
