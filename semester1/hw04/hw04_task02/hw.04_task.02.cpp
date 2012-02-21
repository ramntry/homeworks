// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 04, task 02

// Дан массив размерностью N x N, N - нечетное число.
// Вывести элементы массива при обходе его
// по спирали, начиная с центра

// [time start] 13:50 05.10.11
// [time estimate] 00:30 00


#include <iostream>
#include <iomanip>
#include <cmath>

using namespace std;

void printHelix(int * matrix[], int size)
{
    int x = size / 2;
    int y = x;
    int k = 1;
    cout << "Helix = { ";
    for (;;)
    {
        for (int i = 0; i < k; i++)
        {
           cout << matrix[x][y] << ' ';
           if (k % 2)
           {
               x--;
               if (x == -1)
               {
                   cout << "}" << endl;
                   return;
               }
           }
           else
               x++;
        }
        for (int j = 0; j < k; j++)
        {
           cout << matrix[x][y] << ' ';
           if (k % 2)
               y++;
           else
               y--;
        }
        k++;
    }
}

void printMatrix(int * matrix[], int size)
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


int main()
{
    clog << "This program print the helix-presentation of matrix N x N\n"
         << "Enter N (odd): ";
    int n = 0;
    cin >> n;
    if (n % 2 == 0 || n < 0)
    {
        cout << "N must be odd and positive!" << endl;
        return 1;
    }
    cout << endl;

    int ** matrix = new int*[n];
    for (int i = 0; i < n; i++)
    {
        matrix[i] = new int[n];
        for (int j = 0; j < n; j++)
            matrix[i][j] = i * n + j;
    }

    printMatrix(matrix, n);

    printHelix(matrix, n);

    for (int i = 0; i < n; i++)
        delete[] matrix[i];
    delete[] matrix;

    return 0;
}

// [time done] 14:49 05.10.11
// [time real] 00:59 00

