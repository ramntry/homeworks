#pragma once
#include "list.h"

class ArrayList : public List
{
public:
    ArrayList();
    ~ArrayList();
    void insert(int position, ListItem item);
    void remove(int position);
    int find(ListItem item);
    int length();
    ListItem & operator [](int position);

private:
    int mLength;
};
