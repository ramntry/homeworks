#pragma once
#include "list.h"

class LinkedList : public List
{
public:
    LinkedList();
    void insert(int position, ListItem item);
    void remove(int position);
    int find(ListItem item);
    size_t length();
    ListItem & operator [](int position);
    ~LinkedList();
};
