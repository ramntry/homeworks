#pragma once

typedef int ListItem;
const int itemNotFound = -1;

class List
{
public:
    virtual void insert(int position, ListItem item) = 0;
    virtual void remove(int position) = 0;
    virtual int find(ListItem item) = 0;
    virtual size_t length() = 0;
    virtual ListItem & operator [](int position) = 0;
};

class ListOutOfBoundsException {};
class ListOverflow {};
