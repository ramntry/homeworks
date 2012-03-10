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

//Test heir:
template <typename T, typename C = StandartComparator<T> >
class BubbleSorter : public Sorter<T, C>
{
public:
    void _sort(T* data, size_t size, Comparator<T> &comp)
    {
        for (int i = size - 1; i > 0; --i)
            for (int j = 0; j < i; j++)
                if (comp(data[j], data[j + 1]) > 0) // [4]
                    swap(data[j], data[j + 1]);
    }
};
