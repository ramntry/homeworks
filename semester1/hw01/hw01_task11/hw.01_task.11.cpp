// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 01, task 11

// Реализовать быструю сортировку
// (в рекурсивном варианте)

// [time start] hh:mm dd.mm.11
// [time estimate] hh:mm dd


#include <iostream>

using namespace std;

template <class T>
void quickSort(T * array, int size, int begin, int end)
{
    while (array[begin] < array[0] && end - begin > 1)
        begin++;
    while (array[end] >= array[0] && end - begin > 1)
        end--;

    if (array[begin] > array[end])
    {
        register T t = array[begin];
        array[begin] = array[end];
        array[end] = t;
    }

    if (size < 3)
        return;

    if (end - begin == 1)
    {
        quickSort(array, begin + 1, 0, begin);
        quickSort(&array[end], size - end, 0, size - end - 1);
    }
    else
        quickSort(array, size, begin, end);
}


int main()
{
    int size = 0;
    clog << "Enter size: ";
    cin >> size;
    clog << "Enter array: ";
    int * array = new int[size];
    for (int i = 0; i < size; i++)
        cin >> array[i];

    quickSort(array, size, 0, size - 1);

    for (int i = 0; i < size; i++)
        cout << array[i] << ' ';
    cout << endl;

    delete[] array;
    
    return 0;
}


// [time done] hh:mm dd.mm.11
// [time real] hh:mm dd

