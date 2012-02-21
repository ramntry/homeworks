// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 01, task 01, v2

// Написать программу, считающую значение формулы
// x^4 + x^3 + x^2 + x + 1 за два умножения.

// [time start] 18:53 17.09.11
// [time estimate] 00:15 00


#include <iostream>

using namespace std;

int main()
{
    double x = 0.0;
    cout << "This program calculates x^4 + x^3 + x^2 + x + 1\n"
         << "Enter\tx: ";
    cin >> x;

    double y = x * x;
    cout << "x^4 + x^3 + x^2 + x + 1 = "
         << (y + x) * (y + 1) + 1
         << endl;

    return 0;
}


// [time done] 19:01 17.09.11
// [time real] 00:08 00

