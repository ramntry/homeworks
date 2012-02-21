#include <iostream>
#include <fstream>
#include <limits>

using namespace std;

int **newAdjacencyMatrix(int numTowns)
{
    int **rows = new int *[numTowns];
    for (int i = 0; i < numTowns; i++)
    {
        rows[i] = new int[numTowns];
        for (int j = 0; j < numTowns; j++)
            rows[i][j] = 0;
    }
    return rows;
}

void fillAdjacencyMatrix(int **matrix, int numWays, istream &in)
{
    for (int n = 0, i = 0, l = 0, j = 0; n < numWays; n++)
    {
        in >> i >> j >> l;
        matrix[i - 1][j - 1] = l;  // Нумерация на входе с 1
        matrix[j - 1][i - 1] = l;
    }
}

void deleteAdjacencyMatrix(int **matrix, int numTowns)
{
    for (int i = 0; i < numTowns; i++)
        delete[] matrix[i];
    delete[] matrix;
}

int getNearest(int *distances, bool *seen, int numTowns)
{
    int nearest = 0;
    while (seen[nearest])  // Найдем первую непросмотренную вершину
        nearest++;
    for (int i = nearest; i < numTowns; i++) // среди всех непросмотренных найдем ближайшую
        if (!seen[i] && (distances[i] <  distances[nearest]))
                nearest = i;
    return nearest;
}

void dijkstraPrint(int **adjacencyMatrix, int numTowns)
{
    int *distances = new int[numTowns];
    bool *seen = new bool[numTowns];
    for (int i = 0; i < numTowns; i++)
    {
        seen[i] = false;
        distances[i] = numeric_limits<int>::max();
    }
    distances[0] = 0;

    for (int i = 0; i < numTowns; i++)
    {
        int current = getNearest(distances, seen, numTowns);  // получить ближайшую вершину из фронта сканирования
        seen[current] = true;

        cout << (current + 1) << " (" << distances[current] << ")" << endl;

        for (int adj = 0; adj < numTowns; adj++)
            if (adjacencyMatrix[current][adj] && !seen[adj])    // Для всех вершин, смежных со сканируемой
                // Обновить путь, если путь через сканируемую в текущую смежную короче, чем он у смежной уже есть
                if (distances[current] + adjacencyMatrix[current][adj] < distances[adj])
                    distances[adj] = distances[current] + adjacencyMatrix[current][adj];
    }
    delete[] distances;
    delete[] seen;
}

int main(void)
{
    clog << "Homework 8, task 2. Dijkstra.\nEnter filename: ";
    string fname = "map";
    //cin >> fname;
    ifstream in(fname.c_str());
    if (!in)
    {
        cerr << "File not found" << endl;
        return 1;
    }

    int numTowns = 0;
    in >> numTowns;
    int numWays = 0;
    in >> numWays;

    int **matrix = newAdjacencyMatrix(numTowns);
    fillAdjacencyMatrix(matrix, numWays, in);
    in.close();

    dijkstraPrint(matrix, numTowns);

    deleteAdjacencyMatrix(matrix, numTowns);

    return 0;
}
