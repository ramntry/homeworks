#pragma once
#include <stdexcept>
#include "../../hw02/hw02_task01/list.h"

class InsertingExistingItemException : public std::invalid_argument
{
public:
    InsertingExistingItemException(std::string const& what_arg)
        : std::invalid_argument(what_arg)
    {}
};

class RemovingNonExistingItemException : public std::invalid_argument
{
public:
    RemovingNonExistingItemException (std::string const& what_arg)
        : std::invalid_argument(what_arg)
    {}
};

template <typename Impl>
class UniqueList : public List
{
public:
    UniqueList()
        : d(new Impl)
    {}

    ~UniqueList()
    { delete d; }

    void insert(int position, ListItem item);
    void removeItem(ListItem item);

    void remove(int position)
    { d->remove(position); }

    int find(ListItem item) const
    { return d->find(item); }

    ListItem & at(int position)
    { return d->at(position); }

    int length() const
    { return d->length(); }

private:
    Impl *d;
};

template <typename Impl>
void UniqueList<Impl>::insert(int position, ListItem item)
{
    if (d->find(item) != List::itemNotFound)
        throw InsertingExistingItemException("This item is already in the UniqueList");

    d->insert(position, item);
}

template <typename Impl>
void UniqueList<Impl>::removeItem(ListItem item)
{
    int pos = d->find(item);
    if (pos == List::itemNotFound)
        throw RemovingNonExistingItemException("This item not in the UniqueList");

    d->remove(pos);
}
