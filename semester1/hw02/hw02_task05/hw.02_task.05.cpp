// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 02, task 05

// Реализовать алгоритм пирамидальной сортировки

// [time start] 22:38 24.09.11
// [time estimate] 01:00 00


#include <iostream>

using namespace std;

/*template <class T>                // Аналогичная функция есть в std
inline void swap(T &left, T &right)
{
    T temp = left;
    left = right;
    right = temp;
}*/

template <class T>
void heapify(T * heap, int heapSize, int top)
{
    bool isOK = false;
    int left = (top << 1) + 1;
    int right = left + 1;
    int large = top;

    while (left < heapSize && !isOK)
    {
        large = heap[left] > heap[top] ?
                    left  :      top;
        if (right < heapSize && heap[right] > heap[large])
            large = right;
        if (large != top)
        {
            swap(heap[top], heap[large]);

            top = large;
            left = (top << 1) + 1;
            right = left + 1;
        } else
            isOK = true;
    }
}

template <class T>
inline void createHeap(T * array, int arraySize)
{
    for (int i = arraySize / 2; i >= 0; i--)
        heapify(array, arraySize, i);
}

template <class T>
void heapSort(T * array, int arraySize)
{
    createHeap(array, arraySize);
    int heapSize = arraySize;
    while (heapSize != 1)
    {
        swap(array[0], array[heapSize - 1]);

        heapSize--;
        heapify(array, heapSize, 0);
    }
}


int main()
{
    clog << "This program sorts array of numbers\nEnter size of array: ";
    int size = 0;
    cin >> size;
    clog << "Type values:  ";
    double * array = new double[size];
    for (int i = 0; i < size; i++)
        cin >> array[i];

    heapSort(array, size);

    clog << "Sorted array: ";
    for (int i = 0; i < size; i++)
        cout << array[i] << ' ';
    cout << endl;

    return 0;
}






// [time done] 00:28 24.09.11
// [time real] 01:50 00

