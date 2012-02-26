#include "linkedlist.h"

LinkedList::LinkedList()
{
}

void LinkedList::insert(int position, ListItem item)
{
}

void LinkedList::remove(int position)
{
    throw ListOutOfBoundsException();
}

int LinkedList::find(ListItem item)
{
    return itemNotFound;
}

size_t LinkedList::length()
{
    return 0;
}

ListItem & LinkedList::operator [](int position)
{
    throw ListOutOfBoundsException();
}
