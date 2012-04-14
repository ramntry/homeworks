#pragma once
#include <QtCore/QString>
#include "HashTable.h"

// первый символ строки
class QStringHasherFirstChar : public HashTable<QString>::Hasher
{
public:
    int operator ()(QString const& item)
    {
        return item[0].toAscii();
    }
};

// произведение первых двух символов строки
class QStringHasherTwoChars: public HashTable<QString>::Hasher
{
public:
    int operator ()(QString const& item)
    {
        return item[0].toAscii() * (item.length() > 1 ? item[1].toAscii() : 1);
    }
};

// многочлен от всех символов строки
class QStringHasher : public HashTable<QString>::Hasher
{
public:
    int operator ()(QString const& item)
    {
        unsigned long long hash = 0;
        for (int i = 0; i < item.length(); ++i)
        {
            hash *= 353; // prime number
            hash += item[i].unicode();
        }
        return hash;
    }
};
