#pragma once
#include "BinarySearchTree.h"

template <typename T>
class Set : public BinarySearchTree<T>
{
    struct Dublicator
    {
        bool operator ()(T const& item)
        {
            return true;
        }
    };

    struct Differentiator
    {
        Differentiator(Set<T> &_arg)
            : arg(_arg)
        {}
        bool operator ()(T const& item)
        {
            return !arg.has(item);
        }
        Set<T> &arg;
    };

    struct Intersector
    {
        Intersector(Set<T> &_arg)
            : arg(_arg)
        {}
        bool operator ()(T const& item)
        {
            return arg.has(item);
        }
        Set<T> &arg;
    };

    template <typename F>
    struct FilterInserter
    {
        FilterInserter(Set<T> &_dst, F _filter)
            : dst(_dst)
            , filter(_filter)
        {}

        void operator ()(T &item)
        {
            if (filter(item))
                dst.add(item);
        }

        Set<T> &dst;
        F filter;
    };

public:
    Set<T> setIntersection(Set<T> &set);
    Set<T> setUnion(Set<T> &set);
};

template <typename T>
Set<T> Set<T>::setIntersection(Set<T> &set)
{
    Set<T> res;

    FilterInserter<Intersector> inserter(res, Intersector(set));
    BinarySearchTree<T>::tree->symmetric(inserter);

    return res;
}

template <typename T>
Set<T> Set<T>::setUnion(Set<T> &set)
{
    Set<T> res;

    FilterInserter<Dublicator> dublicator(res, Dublicator());
    BinarySearchTree<T>::tree->symmetric(dublicator);

    FilterInserter<Differentiator> differentiator(res, Differentiator(res));
    set.tree->symmetric(differentiator);

    return res;
}
