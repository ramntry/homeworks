#pragma once

typedef int ListItem;

class List
{
public:
    virtual ~List() {}

    virtual void insert(int position, ListItem item) = 0;
    virtual void remove(int position) = 0;
    virtual ListItem & at(int position) = 0;

    virtual int find(ListItem item) const = 0;
    virtual int length() const = 0;

    static const int itemNotFound = -1;
};

class ListOutOfBoundsException {};
