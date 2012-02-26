#include "arraylist.h"

ArrayList::ArrayList() : mLength(0)
{
}

void ArrayList::insert(int position, ListItem item)
{
    if (position > mLength)
        throw ListOutOfBoundsException();

    mLength++;
}

void ArrayList::remove(int position)
{
    if (position >= mLength)
        throw ListOutOfBoundsException();

    mLength--;
}

int ArrayList::find(ListItem item)
{
    return itemNotFound;
}

int ArrayList::length()
{
    return mLength;
}

ListItem & ArrayList::operator [](int position)
{
    throw ListOutOfBoundsException();
}

ArrayList::~ArrayList()
{
}
