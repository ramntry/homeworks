#include "tests.h"

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

template <template <typename T, typename C = StandartComparator<T> > class S>
void abstractSortTest(const char *Sname, double *a, int size)
{
    int mod = 10;
    double eps = 2.0;
    double eps2 = 1.0;

    fillArray(a, size, mod);
    cout << Sname << " Test[1]:\nBefore:\t\t";
    printArray(a, size);

    Sorter<double> *sorter = new S<double>();
    sorter->sort(a, size);
    cout << "After (eps = " << sorter->comparator().eps() << "):\t";
    printArray(a, size);

    fillArray(a, size, mod);
    cout << "\n" << Sname << " Test[2]:\nBefore:\t\t";
    printArray(a, size);

    sorter->comparator().setEps(eps);
    sorter->sort(a, size);
    cout << "After (eps = " << sorter->comparator().eps() << "):\t";
    printArray(a, size);

    StandartComparator<double> comparator2(eps2);
    sorter->sort(a, size, &comparator2);
    cout << "After (eps = " << comparator2.eps() << "):\t";
    printArray(a, size);
    cout << "\n" << endl;

    delete sorter;
}

void bubbleSortTest(double *a, int size)
{
    abstractSortTest<BubbleSorter>("BubbleSorter", a, size);
}

void heapSortTest(double *a, int size)
{
    abstractSortTest<HeapSorter>("HeapSorter", a, size);
}

void sortersTest()
{
    int size = 10;
    double *a = new double[size];

    bubbleSortTest(a, size);
    heapSortTest(a, size);

    delete[] a;
}
