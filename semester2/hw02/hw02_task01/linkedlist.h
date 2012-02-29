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
    int find(ListItem item);
    int length();
    ListItem & at(int position);

private:
    class LinkedListNode
    {
    public:
        LinkedListNode() :
            mNext(NULL)
        {}

        LinkedListNode(ListItem value, LinkedListNode *next) :
            mValue(value),
            mNext(next)
        {}

        ListItem mValue;
        LinkedListNode *mNext;

    } *mHead;
    int mLength;

    LinkedListNode *getNode(int position);
};
