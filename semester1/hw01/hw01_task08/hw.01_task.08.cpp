// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 01, task 08

// Реализовать подсчет факториала
// (рекурсивно и итеративно)

// [time start] 02:53 18.09.11
// [time estimate] 00:25 00


#include <iostream>

using namespace std;

int factR(int n)
{
    if (n == 0 || n == 1)
        return 1;
    return n * factR(n - 1);
}

int factI(int n)
{
    int f = 1;
    for (int i = 2; i <= n; i++)
        f *= i;
    return f;
}

int main()
{
    cout << "This program calculates the factorial\nEnter n: ";
    int n = 0;
    cin >> n;

    int f = factR(n);
    if (f == factI(n))
        cout << "n! = " << f << endl;
    else
        cout << "Error!" << endl;
    
    return 0;
}

// [time done] 03:03 18.09.11
// [time real] 00:10 00

