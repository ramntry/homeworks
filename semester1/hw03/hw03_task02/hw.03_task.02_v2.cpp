// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 03, task 02

// "Считалочка" — за каждый раунд из циклического списка удаляем
// k-й элемент, пока в списке не останется только один. Вывести значения
// k, при которых последним останется i-й элемент изначального списка

// [time start] 10:00 28.09.11
// [time estimate] hh:mm 00


#include <iostream>
#include "list.h"

using namespace std;

int main()
{
    clog << "Solution of the problem, the inverse problem of Joseph.\n"
         << "Enter the size of a cyclic list (n), and\n"
         << "number of the last element (i): ";
    int n = 0;
    int i = 0;
    cin >> n >> i;

    clog << "k (step of count) in the range [1, n]\n"
         << "can take values: ";

    for (int k = 1; k <= n; k++)
    {
        List<int> list;
        for (int j = 1; j <= n; j++)
            list.append(j);
        int toDel = k - 1;
        while (list.size() != 1)
        {
            list.erase(toDel);
            toDel = (toDel + k - 1) % list.size();
        }
        if (list.pop() == i)
            cout << k << ' ';
    }
    cout << endl;

    return 0;
}


// [time done] hh:mm 28.09.11
// [time real] hh:mm 00

