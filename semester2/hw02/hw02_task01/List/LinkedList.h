#pragma once
#include <cstdlib>
#include "List.h"

template <typename T>
class LinkedList : public List<T>
{
public:
    LinkedList();
    ~LinkedList();

    void insert(size_t position, T item);
    void remove(size_t position);
    T & at(size_t position);

    int find(T item) const;
    size_t length() const { return mLength; }

private:
    class LinkedListNode
    {
    public:
        LinkedListNode();
        LinkedListNode(T value, LinkedListNode *next);

        T mValue;
        LinkedListNode *mNext;
    };

    LinkedListNode *mHead;
    size_t mLength;

    LinkedListNode *getNode(size_t position);
};

template <typename T>
LinkedList<T>::LinkedListNode::LinkedListNode() :
    mNext(NULL)
{}

template <typename T>
LinkedList<T>::LinkedListNode::LinkedListNode(T value, LinkedListNode *next) :
    mValue(value),
    mNext(next)
{}

template <typename T>
typename LinkedList<T>::LinkedListNode *LinkedList<T>::getNode(size_t position)
{
    LinkedListNode *current = mHead;
    for (size_t i = 0; i < position; i++)
    {
        current = current->mNext;
    }
    return current;
}

template <typename T>
LinkedList<T>::LinkedList() :
    mHead(new LinkedList<T>::LinkedListNode()),
    mLength(0)
{}

template <typename T>
LinkedList<T>::~LinkedList()
{
    LinkedListNode *current = mHead;
    while (current != NULL)
    {
        LinkedListNode *tmp = current->mNext;
        delete current;
        current = tmp;
    }
}

template <typename T>
void LinkedList<T>::insert(size_t position, T item)
{
    if (position > mLength)
        throw ListOutOfBoundsException();

    LinkedListNode *prev = getNode(position);
    LinkedListNode *tmp = prev->mNext;
    prev->mNext = new LinkedListNode(item, tmp);

    mLength++;
}

template <typename T>
void LinkedList<T>::remove(size_t position)
{
    if (position >= mLength)
        throw ListOutOfBoundsException();

    LinkedListNode *prev = getNode(position);
    LinkedListNode *tmp = prev->mNext;
    prev->mNext = tmp->mNext;
    delete tmp;

    mLength--;
}

template <typename T>
T & LinkedList<T>::at(size_t position)
{
    if (position >= mLength)
        throw ListOutOfBoundsException();

    return getNode(position)->mNext->mValue;
}

template <typename T>
int LinkedList<T>::find(T item) const
{
    LinkedListNode *current = mHead->mNext;
    for (int i = 0; current != NULL; i++)
    {
        if (current->mValue == item)
            return i;
        current = current->mNext;
    }
    return List<T>::itemNotFound;
}
