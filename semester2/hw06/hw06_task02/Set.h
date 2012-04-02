#pragma once
#include "BinarySearchTree.h"

template <typename T, typename C = StandartComparator<T> >
class Set : public BinarySearchTree<T, C>
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
        Differentiator(BinarySearchTree<T, C> &_arg)
            : arg(_arg)
        {}
        bool operator ()(T const& item)
        {
            return !arg.has(item);
        }
        BinarySearchTree<T, C> &arg;
    };

    struct Intersector
    {
        Intersector(BinarySearchTree<T, C> &_arg)
            : arg(_arg)
        {}
        bool operator ()(T const& item)
        {
            return arg.has(item);
        }
        BinarySearchTree<T, C> &arg;
    };

    template <typename F>
    struct FilterInserter
    {
        FilterInserter(BinarySearchTree<T, C> &_dst, F _filter)
            : dst(_dst)
            , filter(_filter)
        {}

        void operator ()(T &item)
        {
            if (filter(item))
                dst.add(item);
        }

        BinarySearchTree<T, C> &dst;
        F filter;
    };

public:
    Set<T, C> setIntersection(Set<T, C> &set) const;
    Set<T, C> setUnion(Set<T, C> &set) const;
};

template <typename T, typename C>
Set<T, C> Set<T, C>::setIntersection(Set<T, C> &set) const
{
    Set<T, C> res;

    FilterInserter<Intersector> inserter(res, Intersector(set));
    BinarySearchTree<T, C>::tree->preorder(inserter);

    return res;
}

template <typename T, typename C>
Set<T, C> Set<T, C>::setUnion(Set<T, C> &set) const
{
    Set<T, C> res;

    FilterInserter<Dublicator> dublicator(res, Dublicator());
    BinarySearchTree<T, C>::tree->preorder(dublicator);

    FilterInserter<Differentiator> differentiator(res, Differentiator(res));
    set.tree->preorder(differentiator);

    return res;
}

void setTestExec(int argc = 0, char **argv = 0);
