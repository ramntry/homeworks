#pragma once

template <class T>
void swap(T &first, T &second)
{
    T tmp(first);
    first = second;
    second = tmp;
}

template <class T>
void bubbleSort(T *array, int size)
{
    for (int i = 0; i < size - 1; i++)
        for (int j = 0; j < size - 1; j++)
            if (array[j] > array[j + 1])
                swap(array[j], array[j + 1]);
}
