#include "stretchablestack.h"

StretchableStack::StackBlock::StackBlock(int newCapacity, StackBlock *prevBl) :
    array(new StackElement[newCapacity]),
    length(0),
    capacity(newCapacity),
    next(NULL),
    prev(prevBl)
{}

StretchableStack::StackBlock::~StackBlock()
{
    delete[] array;
}

StretchableStack::StretchableStack(int capacity) :
    mHeadBlock(new StackBlock(capacity, NULL)),
    mCurrentBlock(mHeadBlock)
{}

StretchableStack::~StretchableStack()
{
    StackBlock *current = mHeadBlock;
    while (current != NULL)
    {
        StackBlock *tmp = current->next;
        delete current;
        current = tmp;
    }
}

void StretchableStack::push(StackElement value)
{
    if (mCurrentBlock->length == mCurrentBlock->capacity)
    {
        if (mCurrentBlock->next == NULL)
            mCurrentBlock->next = new StackBlock(mCurrentBlock->capacity * capacityMultiplier, mCurrentBlock);

        mCurrentBlock = mCurrentBlock->next;
    }

    mCurrentBlock->array[mCurrentBlock->length++] = value;
}

StackElement StretchableStack::pop()
{
    if (mCurrentBlock->length == 0)
        mCurrentBlock = mCurrentBlock->prev;

    if (mCurrentBlock == NULL)
        throw new StackUnderflowException();

    return mCurrentBlock->array[--mCurrentBlock->length];
}

int StretchableStack::size() const
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

bool StretchableStack::isEmpty() const
{
    return mCurrentBlock->length == 0 && mCurrentBlock->prev == NULL;
}
