#pragma once
#include "sorter.h"

template <typename T, typename C = StandartComparator<T> >
class MergeSorter : public Sorter<T, C>
{
public:
    void _sort(T *data, size_t size, Comparator<T> &comp);

protected:
    void merge(T* left, leftSize, T* right, rightSize, T* destination);
};

template <typename T, typename C>
void MergeSorter::_sort(T *data, size_t size, Comparator<T> &comp)
{

}

template <typename T, typename C>
void MergeSorter::merge(T *left, leftSize, T *right, rightSize, T *destination)
{

}
