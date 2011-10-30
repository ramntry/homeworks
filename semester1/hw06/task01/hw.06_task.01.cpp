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
#include <stdexcept>
#include <cstdlib>

using namespace std;

int main()
{
    BinarySearchTree t;
    try { t.min(); }
    catch (std::runtime_error e) { cerr << e.what() << endl; }
    cout << t.search(8) << endl;

    t.insert(6);
    t.max();
    t.insert(6);
    t.insert(3);
    t.insert(5);
    cout << t.search(8) << endl;
    t.insert(5);
    t.insert(8);
    t.insert(7);

    cout << t.search(8) << endl;

    cout << t.min() << endl;
    cout << t.max() << endl;

    const int n = (1 << 4) - 1;
    int * a = new int[n];
    for (int i = 0; i < n; i++)
        a[i] = rand() % (n * n);
    for (int i = 0; i < n; i++)
        cout << a[i] << ' ';
    cout << endl;

    BinarySearchTree t2(a, n);
    cout << t2.min() << ' ' << t2.max() << endl;

    cout << "I'm a homework 06, task 01" << endl;
    return 0;
}


// [time done] hh:mm 30.10.11
// [time real] hh:mm 00
