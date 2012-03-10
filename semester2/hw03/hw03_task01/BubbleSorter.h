#pragma once
#include "sorter.h"

SORTER_METHOD(BubbleSorter)
{
    for (int i = size - 1; i > 0; --i)
        for (int j = 0; j < i; j++)
            if (comp(data[j], data[j + 1]) > 0)
                swap(data[j], data[j + 1]);
}
