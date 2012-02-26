#pragma once
#include "list.h"

class LinkedList : public List
{
public:
    LinkedList();
    ~LinkedList();
    void insert(int position, ListItem item);
    void remove(int position);
    int find(ListItem item);
    int length();
    ListItem & operator [](int position);

private:
    int mLength;
};
