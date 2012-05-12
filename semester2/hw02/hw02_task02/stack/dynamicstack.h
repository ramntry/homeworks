#pragma once
#include "stack.h"
#include "simplestack.h"

template <class T>
class DynamicStack : virtual public Stack<T>, public SimpleStack<T>
{
public:
    typedef SimpleStack<T> super;

    DynamicStack(int capacity = super::stackCapacity
                ,double capacityMultiplier = 2.0)
        : super(capacity)
        , multiplier(capacityMultiplier)
    {}

    void push(T value);

protected:
    double multiplier;
};

template <class T>
void DynamicStack<T>::push(T value)
{
    if (super::mSize == super::mCapacity)
    {
        super::mCapacity *= multiplier;
        T *tmp = new T[super::mCapacity];

        for (int i = 0; i < super::mSize; ++i)
            tmp[i] = super::mArray[i];

        delete[] super::mArray;
        super::mArray = tmp;
    }

    super::mArray[super::mSize++] = value;
}
