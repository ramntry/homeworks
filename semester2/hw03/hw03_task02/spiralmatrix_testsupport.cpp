#include <iostream>
#include <iomanip>
#include <cmath>
#include <ctime>
#include <cstdlib>
#include "spiralmatrix_testsupport.h"

using std::cout;
using std::endl;
using std::setw;

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

void fillMatrix(int **matrix, int size, int mod)
{
    if (!mod)
        mod = size * size;
    srand(time(NULL));
    for (int i = 0; i < size; i++)
        for (int j = 0; j < size; j++)
            matrix[i][j] = random() % mod;
}

void printMatrix(int **matrix, int size, bool onTheSide)
{
    int wide = ceil(log10(size*size)) + 1;
    for (int i = 0; i < size; i++)
    {
        for (int j = 0; j < size; j++)
            cout << setw(wide) << (
                onTheSide ? matrix[j][i] : matrix[i][j]
            );
        cout << endl;
    }
    cout << endl;
}

void eraseMatrix(int **matrix, int size)
{
    for (int i = 0; i < size; i++)
        delete[] matrix[i];
    delete[] matrix;
}
