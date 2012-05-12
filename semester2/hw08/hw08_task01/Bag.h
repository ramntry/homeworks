#pragma once
#include <QtCore/Qt>
#include <iostream>
#include <utility>

class Bag
{
public:
    Bag();
    ~Bag();

    void add(int value);
    void remove(int value);
    void erase(int value);
    bool has(int value);

    size_t size()
    { return mSize; }

protected:
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

int bagTestExec(int argc, char **argv);
