#include "arraylist.h"

ArrayList::ArrayList(int initCapacity) :
    mLength(0),
    mCapacity(initCapacity)
{
    mArray = new ListItem[mCapacity];
}

ArrayList::~ArrayList()
{
    delete[] mArray;
}

void ArrayList::checkCapacity()
{
    if (mLength == mCapacity)
    {
        mCapacity *= capacityMultiplier;
        ListItem *tmp = new ListItem[mCapacity];
        for (int i = 0; i < mLength; i++)
        {
            tmp[i] = mArray[i];
        }
        delete[] mArray;
        mArray = tmp;
    }
}

void ArrayList::insert(int position, ListItem item)
{
    if (position > mLength)
        throw ListOutOfBoundsException();

    checkCapacity();

    for (int i = mLength; i > position; i--)
    {
        mArray[i] = mArray[i - 1];
    }
    mLength++;

    mArray[position] = item;
}

void ArrayList::remove(int position)
{
    if (position >= mLength)
        throw ListOutOfBoundsException();

    for (int i = position; i < mLength - 1; i++)
    {
        mArray[i] = mArray[i + 1];
    }
    mLength--;
}

int ArrayList::find(ListItem item)
{
    for (int i = 0; i < mLength; i++)
    {
        if (mArray[i] == item)
            return i;
    }
    return itemNotFound;
}

int ArrayList::length()
{
    return mLength;
}

ListItem & ArrayList::at(int position)
{
    if (position >= mLength)
        throw ListOutOfBoundsException();
    return mArray[position];
}
