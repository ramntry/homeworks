#pragma once

template <typename T>
class List
{
public:
    virtual ~List() {}

    virtual void insert(size_t position, T item) = 0;
    virtual void remove(size_t position) = 0;
    virtual T & at(size_t position) = 0;

    virtual int find(T item) const = 0;
    virtual size_t length() const = 0;

    static const int itemNotFound = -1;
};

class ListOutOfBoundsException {};
