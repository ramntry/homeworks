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
        Differentiator(BinarySearchTree<T> &_arg)
            : arg(_arg)
        {}
        bool operator ()(T const& item)
        {
            return !arg.has(item);
        }
        BinarySearchTree<T> &arg;
    };

    struct Intersector
    {
        Intersector(BinarySearchTree<T> &_arg)
            : arg(_arg)
        {}
        bool operator ()(T const& item)
        {
            return arg.has(item);
        }
        BinarySearchTree<T> &arg;
    };

    template <typename F>
    struct FilterInserter
    {
        FilterInserter(BinarySearchTree<T> &_dst, F _filter)
            : dst(_dst)
            , filter(_filter)
        {}

        void operator ()(T &item)
        {
            if (filter(item))
                dst.add(item);
        }

        BinarySearchTree<T> &dst;
        F filter;
    };

public:
    Set<T> setIntersection(Set<T> &set) const;
    Set<T> setUnion(Set<T> &set) const;
};

template <typename T>
Set<T> Set<T>::setIntersection(Set<T> &set) const
{
    Set<T> res;

    FilterInserter<Intersector> inserter(res, Intersector(set));
    BinarySearchTree<T>::tree->symmetric(inserter);

    return res;
}

template <typename T>
Set<T> Set<T>::setUnion(Set<T> &set) const
{
    Set<T> res;

    FilterInserter<Dublicator> dublicator(res, Dublicator());
    BinarySearchTree<T>::tree->symmetric(dublicator);

    FilterInserter<Differentiator> differentiator(res, Differentiator(res));
    set.tree->symmetric(differentiator);

    return res;
}
