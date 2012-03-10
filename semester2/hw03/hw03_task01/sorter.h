#pragma once
#include "comparator.h"

template <typename E>
inline void swap(E &a, E &b)
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

#define SORTER_METHOD(ClassName) \
template <typename T, typename C = StandartComparator<T> >              \
class ClassName : public Sorter<T, C>                                   \
{                                                                       \
public:                                                                 \
    void _sort(T* data, size_t size, Comparator<T> &comp);              \
};                                                                      \
                                                                        \
template <typename T, typename C>                                       \
void ClassName<T, C>::_sort(T* data, size_t size, Comparator<T> &comp)
