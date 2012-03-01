#pragma once
#include "list.h"

class ArrayList : public List
{
public:
    ArrayList(int initCapacity = defaultArrayListCapacity);
    ~ArrayList();

    void insert(int position, ListItem item);
    void remove(int position);
    ListItem & at(int position);

    int find(ListItem item) const;
    int length() const { return mLength; }

private:
    void checkCapacity();

    static const int defaultArrayListCapacity = 256;
    static const int capacityMultiplier = 2;

    int mLength;
    int mCapacity;
    ListItem *mArray;
};
