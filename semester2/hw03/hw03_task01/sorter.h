#pragma once
#include "comparator.h"

template <typename E>
void swap(E &a, E &b)
{
    E c(a);
    a = b;
    b = c;
}

template <typename T, typename C = StandartComparator<T> >
class Sorter
{
public:
    Sorter()
        : mComparator(new C())
    {}
    ~Sorter() { delete mComparator; }
    virtual void sort(T* data, size_t size) = 0;
    C comparator() { return *mComparator; }

private:
    C *mComparator;
};

//Test heir:
template <typename T, typename C = StandartComparator<T> >
class BubbleSorter : public Sorter<T, C>
{
public:
    BubbleSorter()
        : Sorter<T, C>()
    {}

    void sort(T* data, size_t size)
{
        for (int i = size - 1; i > 0; --i)
            for (int j = 0; j < i; j++)
        // ERROR: Строкой ниже не удается обратиться к методу comparator родительского класса напрямую. Я не могу
        //        понять, почему это так происходит.
                if (Sorter<T, C>::comparator()(data[j], data[j + 1]) > 0)
                    swap(data[j], data[j + 1]);
    }
};
