// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 01, task 07

// Написать программу, печатающую все простые числа, не
// превосходящие заданного числа.

// [time start] 2:27 18.09.11
// [time estimate] 00:30 00


#include <iostream>

using namespace std;

int * eratosthenes(int n)
{
    int * array = new int[n];
    for (int i = 0; i < n; i++)
        array[i] = 0;
    
    for (int i = 2; i < n; i++)
       if (array[i] == 0)
       {
           array[++array[0]] = i; //  найденные простые сразу переписываем
           for (int j = 2*i; j < n; j += i)  // в начало массива
               array[j] = 1; 
       }
    return array;
}

int main()
{
    int n = 0;
    cout << "This program is printing prime numbers "
         << "not exceeding a given n\nEnter\tn: ";
    cin >> n;

    int * primes = eratosthenes(n + 1); // не превосходящие - может быть ==
    for (int i = 0; i < primes[0];)
        cout << primes[++i] << ' ';
    delete[] primes;
    cout << endl;

    return 0;
}


// [time done] 2:50 18.09.11
// [time real] 00:23 00

