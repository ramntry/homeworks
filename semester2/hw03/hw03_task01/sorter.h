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
    C &comparator() { return *mComparator; }

    void sort(T* data, size_t size, Comparator<T> *comp = 0)
    { _sort(data, size, comp ? *comp : *mComparator); }

protected:
    virtual void _sort(T* data, size_t size, Comparator<T> &comp) = 0;
    C *mComparator;
};
