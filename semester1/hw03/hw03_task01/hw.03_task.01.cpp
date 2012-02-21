// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 03, task 01

// Найдите максимальный элемент массива, встречающийся более
// одного раза (массив неупорядоченный)

// [time start] 20:39 28.09.11
// [time estimate] 00:20 00


#include <iostream>
#include "quicksort2_impl.h"

using namespace std;

template <class T>
int doubleMax(T * array, int size)
{
    quickSort(array, size);
    for (int i = size - 1; i > 0; --i)
    {
        if (array[i] == array[i - 1])
            return i;
    }
    return -1;
}

int main()
{
    clog << "This program finds the maximum element of the sequence "
         << "occurring more than once\nEnter size of sequence: ";
    int size = 0;
    cin >> size;

    double * array = new double[size];
    clog << "Enter elements: ";
    for (int i = 0; i < size; ++i)
        cin >> array[i];

    int indexMax = doubleMax(array, size);

    if (indexMax != -1)
        cout << "Double max-element is " << array[indexMax] << endl;
    else
        cout << "Double max-element not found!" << endl;


    delete[] array;
    return 0;
}


// [time done] 20:59 28.09.11
// [time real] 00:20 00

