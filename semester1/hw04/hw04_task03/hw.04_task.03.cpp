// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 04, task 03

// Написать программу, которая в диалоговом режиме позволяет
// осуществлять следующие операции:
// 0 - exit
// 1 - add value to sorted list
// 2 - remove value from list
// 3 - print list
// Все операции должны сохранять сортированность. Начинаем с пустого списка.

// [time start] 22:07 11.10.11
// [time estimate] 00:35 00


#include <iostream>
#include "list.h"

using namespace std;

int main()
{
    clog << "This program provides a container of integers, which support "
         << "some operations:\n"
         << "\t1 elem - Add <elem> to container in sorted order\n"
         << "\t2 elem - Find and remove <elem> from container\n"
         << "\t3      - Print container\n"
         << "\t0      - (or ctrl + D) Exit"
         << endl;

    List<int> list;
    int elem = 0;
    char command = '\0';
    clog << "> ";
    cin >> command;
    while (command != '0' && cin)
    {
        if (command == '3')
        {
            clog << ": ";
            list.out();
        }
        else
        {
            cin >> elem;
            if (command == '1')
            {
                int i = 0;
                for (; i < list.size() && list[i] < elem; i++);
                if (i < list.size())
                   list.insert(i, elem);
                else
                    list.append(elem);
            }
            else if (command == '2')
            {
                int index = list.find(elem);
                if (index != -1)
                    list.erase(index);
                else
                    cerr << ": Not in list" << endl;
            }
            else
                cerr << ": Bad command" << endl;
        }
        clog << "> ";
        cin >> command;
    }
    
    clog << "Bye" << endl;

    return 0;
}


// [time done] hh:mm 11.10.11
// [time real] hh:mm 00

