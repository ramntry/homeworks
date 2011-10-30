#pragma once
#include "rbtree.h"

class Set : private RBTree
{
public:
    Set();

    Set &operator <<(int item);
    void exclude(int item);
    bool has(int item) const;
    Set &operator &(const Set &right) const;  // Пересечение
    Set &operator |(const Set &right) const;  // Объединение
    Set &operator /(const Set &right) const;  // Разность
    Set &operator ^(const Set &right) const;  // Симметрическая разность
};
