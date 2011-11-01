#pragma once
#include "rbtree.h"

class Set : private RBTree
{
public:
    Set();

    /* Операции над элементами множества */
    void include(int key);                   ///< Включение
    void exclude(int key);                   ///< Исключение
    bool has(int key) const;                 ///< Проверка на наличие

    /* Операции над множествами */
    Set &intersection(const Set &right) const;          ///< Пересечение
    Set &merger(const Set &right) const;                ///< Объединение
    Set &difference(const Set &right) const;            ///< Разность
    Set &symmetric_difference(const Set &right) const;  ///< Симметрическая разность
};
