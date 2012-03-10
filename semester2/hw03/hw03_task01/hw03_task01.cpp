#include <iostream>
#include <iomanip>
#include <string>
#include <ctime>
#include <cstdlib>
#include "comparator.h"
#include "BubbleSorter.h"

using namespace std;

void comparatorsTest()
{
    cout << "(5, 2):\t\t\t"                          << StandartComparator<int>()(5, 2) << endl;
    cout << "<string>(\"hi\", \"hi\"):\t\t"          << StandartComparator<string>()("hi", "hi") << endl;
    cout << "<const char*>(\"hello\", \"world\"):\t" << StandartComparator<const char*>()("hello", "world") << endl;

    const int comparatorsLength = 3;
    StandartComparator<double> *comparators[comparatorsLength];
    comparators[0] = new StandartComparator<double>;
    comparators[2] = new StandartComparator<double>(0.001);
    comparators[1] = new StandartComparator<double>(0.01);

    for (int i = 0; i < comparatorsLength; i++)
        cout << "(" << comparators[i]->eps() << ")"
             << "(2./3., 2.01/3.):\t" << (*comparators[i])(2./3., 2.01/3.) << endl;

    for (int i = 0; i < comparatorsLength; i++)
        delete comparators[i];
}

void fillArray(double *a, int size, int mod)
{
    srand(time(0));
    for (int i = 0; i < size; i++)
        a[i] = random() % mod + 1.0/mod * (random() % mod);
}

void printArray(double *a, int size)
{
    for (int i = 0; i < size; i++)
        cout << a[i] << "  ";
    cout << endl;
}

void bubbleSortTest(double *a, int size)
{
    int mod = 10;
    double eps = 2.0;
    double eps2 = 1.0;

    fillArray(a, size, mod);
    cout << "BubbleSort Test[1]:\nBefore:\t\t";
    printArray(a, size);

    Sorter<double> *sorter = new BubbleSorter<double>();
    sorter->sort(a, size);
    cout << "After (eps = " << sorter->comparator().eps() << "):\t";
    printArray(a, size);

    fillArray(a, size, mod);
    cout << "\nBubbleSort Test[2]:\nBefore:\t\t";
    printArray(a, size);

    sorter->comparator().setEps(eps);
    sorter->sort(a, size);
    cout << "After (eps = " << sorter->comparator().eps() << "):\t";
    printArray(a, size);

    StandartComparator<double> comparator2(eps2);
    sorter->sort(a, size, &comparator2);
    cout << "After (eps = " << comparator2.eps() << "):\t";
    printArray(a, size);

    delete sorter;
}

void sortersTest()
{
    int size = 10;
    double *a = new double[size];

    bubbleSortTest(a, size);

    delete[] a;
}

int main()
{
    cout << "\t\t\tTesting of comparators:\n" << endl;
    comparatorsTest();

    cout << "\n\n\t\t\tTesting of sorters:\n" << endl;
    sortersTest();

    return 0;
}
