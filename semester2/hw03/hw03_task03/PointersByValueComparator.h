#pragma once
#include "../hw03_task01/comparator.h"

template <typename T>
class PointersByValueComparator : public Comparator<T>
{
public:
    int operator ()(T const& a, T const& b)
    {
        if (*a < *b)
            return -1;
        if (*a > *b)
            return 1;
        return 0;
    }
};
