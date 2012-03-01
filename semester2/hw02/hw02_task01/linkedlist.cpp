#include "linkedlist.h"

LinkedList::LinkedListNode::LinkedListNode() :
    mNext(NULL)
{}

LinkedList::LinkedListNode::LinkedListNode(ListItem value, LinkedListNode *next) :
    mValue(value),
    mNext(next)
{}

LinkedList::LinkedListNode *LinkedList::getNode(int position)
{
    LinkedListNode *current = mHead;
    for (int i = 0; i < position; i++)
    {
        current = current->mNext;
    }
    return current;
}

LinkedList::LinkedList() :
    mHead(new LinkedList::LinkedListNode()),
    mLength(0)
{}

LinkedList::~LinkedList()
{
    LinkedListNode *current = mHead;
    while (current != NULL)
    {
        LinkedListNode *tmp = current->mNext;
        delete current;
        current = tmp;
    }
}

void LinkedList::insert(int position, ListItem item)
{
    if (position > mLength)
        throw ListOutOfBoundsException();

    LinkedListNode *prev = getNode(position);
    LinkedListNode *tmp = prev->mNext;
    prev->mNext = new LinkedListNode(item, tmp);

    mLength++;
}

void LinkedList::remove(int position)
{
    if (position >= mLength)
        throw ListOutOfBoundsException();

    LinkedListNode *prev = getNode(position);
    LinkedListNode *tmp = prev->mNext;
    prev->mNext = tmp->mNext;
    delete tmp;

    mLength--;
}

ListItem & LinkedList::at(int position)
{
    if (position >= mLength)
        throw ListOutOfBoundsException();

    return getNode(position)->mNext->mValue;
}

int LinkedList::find(ListItem item) const
{
    LinkedListNode *current = mHead->mNext;
    for (int i = 0; current != NULL; i++)
    {
        if (current->mValue == item)
            return i;
        current = current->mNext;
    }
    return itemNotFound;
}
