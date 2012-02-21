// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 02, task 01

// Реализовать рекурсивный и итеративный
// подсчет чисел Фибоначчи

// [time start] 21:31 21.09.11
// [time estimate] 00:30 00


#include <iostream>
#include <cmath>

using namespace std;

int fibonacciRecur(int n)
{
    if (!n) return 0;
    return n > 1 ? fibonacciRecur(n - 1)
                 + fibonacciRecur(n - 2) : 1;
}

int fibonacciCycle(int n)
{
    int doublePrev = 1;
    int prev = 1;
    int temp = 0;
    while (n > 2)
    {
        temp = prev;
        prev += doublePrev;
        doublePrev = temp;
        n--;
    }
    return prev;
}

int fibonacciMatan(int n)
{
    double sqrt5 = sqrt(5);
    return (int)(floor((pow(((1 + sqrt5)/2.0), n) -
                        pow(((1 - sqrt5)/2.0), n)) / sqrt5));
}

int main()
{
    for (int i = 1;; i++)
    {
        int cycle = fibonacciCycle(i);
        int matan = fibonacciMatan(i);
        if (cycle < 0)         // Здесь видно, что вплоть до пере-
            break;             // полнения int матан-метод абсолютно
        if (cycle != matan)    // точен
        {
            cout << i << ": " << cycle
                 << " - " << matan << endl;
        }
    }

    clog << "This program calculates n-th Fibonacci number\n"
         << "Enter\tn: ";
    int n = 0;
    cin >> n;
    clog << "n-th Fibonacci number: ";
    cout << fibonacciMatan(n) << endl;
    clog << "Recursion variant: (calculate...) ";
    clog << fibonacciRecur(n) << endl;
    return 0;
}


// [time done] hh:mm dd.mm.11
// [time real] hh:mm dd

