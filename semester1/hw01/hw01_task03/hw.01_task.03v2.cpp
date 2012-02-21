// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 01, task 03, v2

// Дан массив целых чисел x[1]...x[m+n], рассматриваемый как
// соединение двух его отрезков: начала x[1]...x[m] длины m
// и конца x[m+1]...x[m+n] длины n. Не используя
// дополнительных массивов, переставить начало и конец.

// [time start] 18:34 17.09.11
// [time estimate] 00:10 00


#include <iostream>

using namespace std;

void reverse(int x[], int begin, int end)
{
    register int t = 0;
    end--;               // end указывает на элемент позади последнего -
    while (end > begin)  // перед работой следует его сдвинуть
    {
        t = x[begin];
        x[begin] = x[end];
        x[end] = t;
        begin++;  // begin сдвигаем здесь - он в порядке
        end--;
    }
}

int main()
{
    int m = 0;
    int n = 0;
    cout << "This program transforms the\narray x: "
         << "m1 m2 m3 ... m | n1 n2 ... n  --\n"
         << "                                         | to\narray y: "
         << "n1 n2 ... n | m1 m2 m3 ... m  <-\n"
         << "Enter\t m n: ";
    cin >> m >> n;
    n += m;  // чистый n нигде не понадобится, но понадобится
             // полный размер массива x

    cout << "Creation x. Enter x1 x2 ... x" << n << ": ";
    int * x = new int[n];
    for (int i = 0; i < n; i++)
        cin >> x[i];

    reverse(x, 0, m);     // переворачиваем каждый кусочек, затем
    reverse(x, m, n);     // весь массив целиком. Не наоборот - 
    reverse(x, 0, n);     // иначе потеряют актуальность m и n

    cout << "array y: ";
    for (int i = 0; i < n; i++)
        cout << x[i] << ' ';
    cout << endl;

    delete[] x;

    return 0;
}

// [time done] 18:49 17.09.11
// [time real] 00:15 00

