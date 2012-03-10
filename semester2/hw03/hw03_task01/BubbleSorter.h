#pragma once
#include "sorter.h"

template <typename T, typename C = StandartComparator<T> >
class BubbleSorter : public Sorter<T, C>
{
public:
    void _sort(T* data, size_t size, Comparator<T> &comp)
    {
        for (int i = size - 1; i > 0; --i)
            for (int j = 0; j < i; j++)
                if (comp(data[j], data[j + 1]) > 0)
                    swap(data[j], data[j + 1]);
    }
};
