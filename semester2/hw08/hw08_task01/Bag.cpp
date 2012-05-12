#include "Bag.h"

Bag::Bag()
    : mRoot(NULL)
    , mSize(0)
{
}

Bag::~Bag()
{
    traversal(mRoot, [](TreapNode *node)
    {
        delete node;
    });
}

Bag::Parts Bag::split(Bag::TreapNode *rootNode, int key, bool strict)
{
    if (rootNode == NULL)
        return Parts(NULL, NULL);

    if (key > rootNode->key || (!strict && key == rootNode->key))
    {
        Parts parts = split(rootNode->rightChild, key, strict);
        rootNode->rightChild = parts.first;
        return Parts(rootNode, parts.second);
    }
    else
    {
        Parts parts = split(rootNode->leftChild, key, strict);
        rootNode->leftChild = parts.second;
        return Parts(parts.first, rootNode);
    }
}

Bag::TreapNode *Bag::merge(Bag::TreapNode *left, Bag::TreapNode *right)
{
    if (left == NULL)
        return right;
    if (right == NULL)
        return left;

    if (left->priority > right->priority ||
        (left->priority == right->priority && left->key < right->key))
    {
        left->rightChild = merge(left->rightChild, right);
        return left;
    }
    else
    {
        right->leftChild = merge(left, right->leftChild);
        return right;
    }
}

void Bag::insert(TreapNode *newNode, Bag::TreapNode *&list)
{
    if (list == NULL || list->priority < newNode->priority)
    {
        newNode->leftChild = list;
        list = newNode;
    }
    else
        insert(newNode, list->leftChild);
}

void Bag::add(int value)
{
    Parts nonStrict = split(mRoot, value);
    Parts strict = split(nonStrict.first, value, true);

    insert(new TreapNode(value), strict.second);

    TreapNode *tmp = merge(strict.first, strict.second);
    mRoot = merge(tmp, nonStrict.second);
    mSize++;
}

void Bag::remove(int value)
{
    Parts nonStrict = split(mRoot, value);
    Parts strict = split(nonStrict.first, value, true);
    TreapNode *toDel = strict.second;

    if (toDel != NULL)
    {
        strict.second = strict.second->leftChild;
        delete toDel;
        mSize--;
    }

    TreapNode *tmp = merge(strict.first, strict.second);
    mRoot = merge(tmp, nonStrict.second);
}

void Bag::erase(int value)
{
    Parts nonStrict = split(mRoot, value);
    Parts strict = split(nonStrict.first, value, true);
    TreapNode *cursor = strict.second;

    while (cursor != NULL)
    {
        TreapNode *toDel = cursor;
        cursor = cursor->leftChild;
        delete toDel;
        mSize--;
    }

    mRoot = merge(strict.first, nonStrict.second);
}

bool Bag::has(int value)
{
    TreapNode *cursor = mRoot;

    while (cursor != NULL && cursor->key != value)
        if (value < cursor->key)
            cursor = cursor->leftChild;
        else
            cursor = cursor->rightChild;

    return cursor != NULL;
}

template <typename F>
void Bag::traversal(TreapNode *rootNode, F function)
{
    if (rootNode == NULL)
        return;

    traversal(rootNode->leftChild, function);
    TreapNode *rightChild = rootNode->rightChild;
    function(rootNode);
    traversal(rightChild, function);
}

std::ostream &operator <<(std::ostream &os, Bag &bag)
{
    os << "Bag( ";
    bag.traversal(bag.mRoot, [&os](Bag::TreapNode *node)
    {
        os << node->key << ' ';
    });

    return os << ')';
}
