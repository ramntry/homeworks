#include <iostream>
#include "bubble.h"

using namespace std;

int main()
{
    clog << "Bubble sorting. Enter a size of array: ";
    int size = 0;
    cin >> size;
    int *array = new int[size];
    clog << "... and numbers:" << endl;
    for (int i = 0; i < size; i++)
        cin >> array[i];


    bubbleSort(array, size);

    for (int i = 0; i < size; i++)
        cout << array[i] << ' ';
    cout << endl;

    delete[] array;
    return 0;
}
