#pragma once
#include <cstdlib>
#include "list.h"

class LinkedList : public List
{
public:
    LinkedList();
    ~LinkedList();

    void insert(int position, ListItem item);
    void remove(int position);
    ListItem & at(int position);

    int find(ListItem item) const;
    int length() const { return mLength; }

private:
    class LinkedListNode
    {
    public:
        LinkedListNode();
        LinkedListNode(ListItem value, LinkedListNode *next);

        ListItem mValue;
        LinkedListNode *mNext;
    };

    LinkedListNode *mHead;
    int mLength;

    LinkedListNode *getNode(int position);
};
