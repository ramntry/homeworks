#pragma once
#include "list.h"

const int defaultArrayListCapacity = 1024;
const int capacityMultiplier = 2;

class ArrayList : public List
{
public:
    ArrayList(int initCapacity = defaultArrayListCapacity);
    ~ArrayList();
    void insert(int position, ListItem item);
    void remove(int position);
    int find(ListItem item);
    int length();
    ListItem & at(int position);

private:
    void checkCapacity();
    int mLength;
    int mCapacity;
    ListItem *mArray;
};
