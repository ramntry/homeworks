#pragma once

template <typename T>
class List
{
public:
    virtual ~List() {}

    virtual void insert(int position, T item) = 0;
    virtual void remove(int position) = 0;
    virtual T & at(int position) = 0;

    virtual int find(T item) const = 0;
    virtual int length() const = 0;

    static const int itemNotFound = -1;
};

class ListOutOfBoundsException {};
