#include <cstdio>

const int numHonestStudents = 3;

inline
char **newAdjacencyMatrix(int numStudents)
{
    char **rows = new char *[numStudents];
    for (int i = 0; i < numHonestStudents; i++)           // Первые numHonestStudents студентов писали сами
        rows[i] = NULL;
    for (int i = numHonestStudents; i < numStudents; i++)
    {
        rows[i] = new char[numStudents];
        for (int j = 0; j < numStudents; j++)
            rows[i][j] = 0;
    }

    return rows;
}

inline
void fillAdjacencyMatrix(char **matrix, int numStudents)
{
    for (int n = numHonestStudents, i = 0, j = 0; n < numStudents; n++)
    {
        scanf("%d%d", &i, &j);
        matrix[i - 1][j - 1] = 1;  // Нумерация на входе с 1
    }
}

inline
void deleteAdjacencyMatrix(char **matrix, int numStudents)
{
    for (int i = numHonestStudents; i < numStudents; i++)
        delete[] matrix[i];
    delete[] matrix;
}

inline
int getPrimarySrc(char **matrix, int nearestSrc)
{
    while (nearestSrc >= numHonestStudents)   // Пока не дошли до первоисточника из числа честных студентов
    {
        int j = 0;
        for (; !matrix[nearestSrc][j]; j++);  // Ищем предыдущий по цепочке источник
        nearestSrc = j;
    }
    return nearestSrc;
}

int main(void)
{
    printf("Homework 7, task 1\nNumber of students = ");
    int numStudents = 0;
    scanf("%d", &numStudents);
    char **matrix = newAdjacencyMatrix(numStudents);

    printf("List of dishonest students with their sources:\n");
    fillAdjacencyMatrix(matrix, numStudents);

    printf("Student Variant\n");
    for (int i = numHonestStudents; i < numStudents; i++)
    {
        int primarySrc = getPrimarySrc(matrix, i);  // В начале поиска ближайшим источником считаем самих себя
        printf("%7d %7d\n", i + 1, primarySrc + 1);
    }

    deleteAdjacencyMatrix(matrix, numStudents);

    return 0;
}
