#pragma once

typedef int ListItem;
const int itemNotFound = -1;

class List
{
public:
    virtual ~List() {}
    virtual void insert(int position, ListItem item) = 0;
    virtual void remove(int position) = 0;
    virtual int find(ListItem item) = 0;
    virtual int length() = 0;
    virtual ListItem & at(int position) = 0;
};

class ListOutOfBoundsException {};
