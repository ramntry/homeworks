// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 02, task 02

// Реализовать подсчет возведение в целую степень
// (с логарифмической сложностью алгоритма)

// [time start] 21:19 21.09.11
// [time estimate] 00:15 00


#include <iostream>

using namespace std;

template <class T>
T pow(T base, int n)
{
    T result = 1;
    while (n != 0)
    {
        if (n & 1)
            result *= base;
        base *= base;
        n >>= 1;
    }
    return result;
}

int main()
{
    clog << "This program calculates base^n\nEnter\t base n: ";
    double base = 0.0;
    int n = 0;
    cin >> base >> n;

    cout << base << '^' << n << " = "
         << pow<double>(base, n) << endl;

    return 0;
}



// [time done] 21:29 21.09.11
// [time real] 00:10 00

