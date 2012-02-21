// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 02, task 03

// Напечатать все представления натурального числа N суммой
// натуральных слагаемых. Перестановка слагаемых нового способа не дает

// [time start] 20:44 24.09.11
// [time estimate] 01:00 00


#include <iostream>

using namespace std;

void printFilter(int * array, int sum)
{
    int index = 0;
    int acc = 0;
    while (acc != sum)
    {
        acc += array[index];
        if (sum != acc && array[index + 1] > array[index])
            return;
        index++;
    }
    for (int i = 0; i < index; i++)
        cout << array[i] << ' ';
    cout << endl;
}

void parts(int * array, int pos, int sum)
{
    int counts = array[pos];
    for (int i = 1; i < counts; i++)
    {
        array[pos]--;
        array[pos + 1] = i;
        printFilter(array, sum);
        parts(array, pos + 1, sum);
    }
}

void printParts(int sum)
{
    cout << sum << endl;
    int * array = new int[sum];
    array[0] = sum;
    parts(array, 0, sum);
    delete[] array;
}

int main()
{
    clog << "This program prints all partitions of natural number n\n"
         << "Enter\tn: ";
    int sum = 0;
    cin >> sum;
    clog << "Parts:" << endl;
    printParts(sum);

    return 0;
}


// [time done] 14:49 04.10.11
// [time real] 18:05 09

