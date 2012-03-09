#include <iostream>
#include <iomanip>
#include <cmath>
#include "spiralmatrix_testsupport.h"

using std::cout;
using std::endl;
using std::setw;

void printMatrix(int **matrix, int size)
{
    int wide = ceil(log10(size*size)) + 1;
    for (int i = 0; i < size; i++)
    {
        for (int j = 0; j < size; j++)
            cout << setw(wide) << matrix[i][j];
        cout << endl;
    }
    cout << endl;    
}

int **createMatrix(int size)
{

    int ** matrix = new int*[size];
    for (int i = 0; i < size; i++)
    {
        matrix[i] = new int[size];
        for (int j = 0; j < size; j++)
            matrix[i][j] = i * size + j;
    }
    return matrix;
}

void eraseMatrix(int **matrix, int size)
{
    for (int i = 0; i < size; i++)
        delete[] matrix[i];
    delete[] matrix;
}

