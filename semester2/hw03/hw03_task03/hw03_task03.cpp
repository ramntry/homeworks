#include <iostream>
#include "../hw03_task02/spiralmatrix_testsupport.h"
#include "../hw03_task01/BubbleSorter.h"
#include "PointersByValueComparator.h"

using namespace std;

typedef PointersByValueComparator<int *> Comp;

int main()
{
    clog << "This program sorts the columns of a matrix of order n of the first element.\nn = ";
    int n = 1;
    cin >> n;
    n = n < 0 ? -n : n;

    int **matrix = createMatrix(n);
    fillMatrix(matrix, n);

    cout << "\nThe matrix before sorting:\n" << endl;
    printMatrix(matrix, n, true);

    Sorter<int *, Comp> *sorter = new BubbleSorter<int *,  Comp>();
    sorter->sort(matrix, n);

    cout << "\nThe matrix after sorting:\n" << endl;
    printMatrix(matrix, n, true);

    eraseMatrix(matrix, n);
    delete sorter;

    return 0;
}
