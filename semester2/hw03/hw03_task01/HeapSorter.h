#pragma once
#include "sorter.h"

template <typename T, typename C = StandartComparator<T> >
class HeapSorter : public Sorter<T, C>
{
public:
    HeapSorter()
        : mData(0)
        , mSize(0)
    {}

    void _sort(T* data, size_t size, Comparator<T> &comp);

private:
    T* mData;
    size_t mSize;

    void heapify(size_t pos, Comparator<T> &comp);
    void makeHeap(Comparator<T> &comp)
    {
        for (int i = (mSize + 1) / 2; i >= 0; --i)
            heapify(i, comp);
    }
};

template <typename T, typename C>
void HeapSorter<T, C>::_sort(T* data, size_t size, Comparator<T> &comp)
{
    mData = data;
    mSize = size;
    makeHeap(comp);

    while (mSize > 1)
    {
        swap(mData[0], mData[--mSize]);
        heapify(0, comp);
    }
}

template <typename T, typename C>
void HeapSorter<T, C>::heapify(size_t pos, Comparator<T> &comp)
{
    size_t greatest = pos;
    for (;;)
    {
        size_t left = (pos + 1) * 2 - 1;
        size_t right = (pos + 1) * 2;

        if (left < mSize)
        {
            if (comp(mData[left], mData[greatest]) > 0)
                greatest = left;

            if (right < mSize && comp(mData[right], mData[greatest]) > 0)
                greatest = right;
        }
        if (pos != greatest)
        {
            swap(mData[pos], mData[greatest]);
            pos = greatest;
        }
        else
            break;
    }
}
