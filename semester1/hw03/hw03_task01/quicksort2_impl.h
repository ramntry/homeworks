template <class T>
void _quickSort(T * array, int size, int begin, int end)
{
    while (array[begin] < array[0] && end - begin > 1)
        begin++;
    while (array[end] >= array[0] && end - begin > 1)
        end--;

    if (array[begin] > array[end])
        swap(array[begin], array[end]);

    if (size < 3)
        return;

    if (end - begin == 1)
    {
        _quickSort(array, begin + 1, 0, begin);
        _quickSort(&array[end], size - end, 0, size - end - 1);
    }
    else
        _quickSort(array, size, begin, end);
}


export template <class T>
void quickSort(T * array, int size)
{
    _quickSort(array, size, 0, size - 1);
}

