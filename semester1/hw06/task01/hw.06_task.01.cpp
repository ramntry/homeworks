// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 06, task 01

// Реализовать АТД "множество" на основе двоичного дерева поиска. Программа должна позволять в интерактивном режиме
// добавлять значения целого типа в множество, удалять значения, проверять, принадлежит ли значение множеству,
// печатать текущие элементы множества в возрастающем и убывающем порядках.

// [time start] 18:27 30.10.11
// [time estimate] hh:mm 00


#include <iostream>
#include "binarysearchtree.h"

using namespace std;

void print(int key)
{
    cout << key << ' ';
}

int main()
{
    BinarySearchTree set;
    clog << "(Type h for help)\n : ";
    char command = '"';
    cin >> command;
    int item = 0;
    while (cin && command != 'q')
    {
        switch (command)
        {
        case '+':
            cin >> item;
            set.insert(item);
            break;
        case '-':
            cin >> item;
            set.remove(item);
            break;
        case '<':
            cout << "<: ";
            set.symorder(print);
            cout << endl;
            break;
        case '>':
            cout << ">: ";
            set.symorderBack(print);
            cout << endl;
            break;
        case '?':
            cin >> item;
            cout << "?: " << (set.hasKey(item) ? 't' : 'f') << endl;
            break;
        case 'h':
            clog << "h: + <item> - Include a new item into set\n"
                 << "   - <item> - Exclude an item from set (t - item is in set, f - isn't in set)\n"
                 << "   ? <item> - Check item in set\n"
                 << "   <        - Print items in increasing order\n"
                 << "   >        -     ...     in decreasing order\n"
                 << "   q        - Exit\n"
                 << "   h        - Print this help"
                 << endl;
            break;
        default:
            cerr << "e: Invalid command. Use +, -, ?, >, <, q or h to help" << endl;
        }
        clog << " : ";
        cin >> command;
    }
    return 0;
}


// [time done] hh:mm 30.10.11
// [time real] hh:mm 00
