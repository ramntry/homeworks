#pragma once
#include "rbtree.h"

class Set : private RBTree
{
public:
    Set();

    /* Операции над элементами множества */
    Set &operator <<(int item);               // Добавление
    void exclude(int item);                   // Исключение
    bool has(int item) const;                 // Проверка на наличие

    /* Операции над множествами */
    Set &operator &(const Set &right) const;  // Пересечение
    Set &operator |(const Set &right) const;  // Объединение
    Set &operator /(const Set &right) const;  // Разность
    Set &operator ^(const Set &right) const;  // Симметрическая разность
};
