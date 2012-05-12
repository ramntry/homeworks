#pragma once
#include <QtCore/Qt>
#include <iostream>
#include <cmath>
#include <utility>
#include "../../hw02/hw02_task02/stack/dynamicstack.h"

class Bag
{
public:
    class Iterator;

    Bag();
    ~Bag();

    void add(int value);
    void remove(int value);
    void erase(int value);
    bool has(int value);

    size_t size()
    { return mSize; }

    Iterator find(int value);
    Iterator begin();

    Iterator end();

protected:
    class AbstractIterator;
    class TraversalIterator;
    class FindIterator;

    struct TreapNode
    {
        TreapNode(int value)
            : leftChild(NULL)
            , rightChild(NULL)
            , priority(qrand())
            , key(value)
        {}

        TreapNode *leftChild;
        TreapNode *rightChild;

        int priority;
        int key;
    };

    typedef std::pair<TreapNode *, TreapNode *> Parts;

    Parts split(TreapNode *rootNode, int key, bool strict = false);
    TreapNode *merge(TreapNode *left, TreapNode *right);
    void insert(TreapNode *newNode, TreapNode *&list);

    template <typename F>
    void traversal(TreapNode *rootNode, F function);

private:
    TreapNode *mRoot;
    size_t mSize;

friend
    std::ostream &operator <<(std::ostream &os, Bag &bag);
};

class Bag::AbstractIterator
{
public:
    AbstractIterator(size_t size)
        : stack(std::min(size, (size_t)(4.5 * log(size + 1))), 1.5)
        , cursor(NULL)
    {}

    int &get()
    { return cursor->key; }

    virtual void next() = 0;
    virtual bool hasNext() = 0;

protected:
    DynamicStack<TreapNode *> stack;
    TreapNode *cursor;

friend
    bool operator ==(Iterator const& l, Iterator const& r);
};

class Bag::Iterator
{
public:
    Iterator()
        : d(NULL)
    {}

    ~Iterator()
    { delete d; }

    int &operator *()
    { return d->get(); }

    void operator ++()
    { d->next(); }

    bool operator !=(Iterator const& with)
    { return !(*this == with); }

protected:
    Iterator(Bag::AbstractIterator *impl)
        : d(impl)
    {}

    AbstractIterator *d;

friend
    bool operator ==(Iterator const& l, Iterator const& r)
    {
        if (r.d == NULL)
            return l.d == NULL || !l.d->hasNext();
        if (l.d == NULL)
            return r.d == NULL || !r.d->hasNext();

        return l.d->cursor == r.d->cursor;
    }

friend class Bag;
};

class Bag::TraversalIterator : public Bag::AbstractIterator
{
public:
    TraversalIterator(size_t size, TreapNode *root)
        : AbstractIterator(size)
        , counter(size)
    {
        cursor = root;
        while (cursor->leftChild)
        {
            stack.push(cursor);
            cursor = cursor->leftChild;
        }
    }

    void next()
    {
        if (--counter == 0)
            return;

        if (cursor->rightChild != NULL)
            down();
        else
            up();
    }

    bool hasNext()
    { return counter > 0; }

protected:
    void up()
    {
        while (stack.look()->rightChild == cursor)
            cursor = stack.pop();
        cursor = stack.pop();
    }

    void down()
    {
        cursor = cursor->rightChild;
        while (cursor->leftChild != NULL)
        {
            stack.push(cursor);
            cursor = cursor->leftChild;
        }
    }

    int counter;
};

class Bag::FindIterator : public Bag::AbstractIterator
{
};

Bag::Iterator Bag::end()
{
    return Iterator();
}

Bag::Iterator Bag::begin()
{
    if (mSize == 0)
        return Iterator();

    return Iterator(new TraversalIterator(mSize, mRoot));
}

int bagTestExec(int argc, char **argv);
