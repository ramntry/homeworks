#include <iostream>
#include <fstream>
#include <iomanip>

using namespace std;

int **newAdjacencyMatrix(int numTowns)
{
    int **rows = new int *[numTowns];
    for (int i = 0; i < numTowns; i++)
    {
        rows[i] = new int[numTowns];
        for (int j = 0; j < numTowns; j++)
        {
            rows[i][j] = 1;
            rows[i][j] <<= 31;
            rows[i][j] = -99;
        }
    }
    return rows;
}

int **copyAdjacencyMatrix(int **matrix, int numTowns)
{
    int **rows = new int *[numTowns];
    for (int i = 0; i < numTowns; i++)
    {
        rows[i] = new int[numTowns];
        for (int j = 0; j < numTowns; j++)
            rows[i][j] = matrix[i][j];
    }

    return rows;
}

void fillAdjacencyMatrix(int **matrix, int numTowns, istream &in)
{
    for (int n = 0, i = 0, l = 0, j = 0; n < numTowns; n++)
    {
        in >> i;
        in >> j;
        in >> l;
        matrix[i - 1][j - 1] = l;  // Нумерация на входе с 1
    }
}

void deleteAdjacencyMatrix(int **matrix, int numTowns)
{
    for (int i = 0; i < numTowns; i++)
        delete[] matrix[i];
    delete[] matrix;
}

void print(int **matrix, int numTowns)
{
    cout << '\n' << endl;
    for (int i = 0; i < numTowns; i++)
    {
        for (int j = 0; j < numTowns; j++)
            cout << setw(4) << matrix[i][j];
        cout << endl;
    }
    cout << '\n' << endl;
}

void floyd(int **matrix, int numTowns)
{
    int **matrices[2];
    matrices[0] = copyAdjacencyMatrix(matrix, numTowns);
    matrices[1] = matrix;

    for (int k = 0; k < numTowns; k++)
    {
        for (int i = 0; i < numTowns; i++)
            for (int j = 0; j < numTowns; j++)
                matrices[k % 2][i][j] = min(matrices[(k + 1) % 2][i][j],
                                            matrices[(k + 1) % 2][i][k] + matrices[(k + 1) % 2][k][j]);
        print(matrix, numTowns);
    }

    deleteAdjacencyMatrix(matrices[0], numTowns);
}

int main(void)
{
    ifstream in("map.txt");
    int numTowns = 0;
    in >> numTowns;
    int numWays = 0;
    in >> numWays;

    int **matrix = newAdjacencyMatrix(numTowns);
    fillAdjacencyMatrix(matrix, numTowns, in);
    in.close();

    print(matrix, numTowns);

    floyd(matrix, numTowns);

    print(matrix, numTowns);
    for (int i = 0; i < numTowns; i++)
        cout << matrix[0][i] << ' ';
    cout << endl;

    deleteAdjacencyMatrix(matrix, numTowns);
    return 0;
}
