#pragma once
#include <cstdlib>

struct BinarySearchNode
{
    int m_key;
    BinarySearchNode *m_parent;
    BinarySearchNode *m_leftChild;
    BinarySearchNode *m_rightChild;

    BinarySearchNode(int key);
};

struct BinarySearchResult
{
    BinarySearchNode *&m_place;
    BinarySearchNode *m_parent;

    BinarySearchResult(BinarySearchNode *&place, BinarySearchNode *parent) :
        m_place(place),
        m_parent(parent)
    {}

    bool operator ==(BinarySearchNode *right) const { return right == m_place; }
    bool operator !=(BinarySearchNode *right) const { return right != m_place; }
};

class BinarySearchTree
{
public:
    BinarySearchTree(BinarySearchNode *root = NULL) : m_root(root) {}
    BinarySearchTree(const int *array, int size);

    bool isEmpty() const { return m_root == NULL; }
    bool hasKey(int key) { return search(key) != NULL; }
    int successor(int key) const { return successorNode(key)->m_key; }
    int predecessor(int key) const { return predecessorNode(key)->m_key; }
    int min() const { return minNode()->m_key; }
    int max() const { return maxNode()->m_key; }

    virtual void insert(int key);
    virtual void remove(int key);

    void symorder(void (*act)(int key));
    void symorderBack(void (*act)(int key));

protected:
    BinarySearchResult search(int key);

    BinarySearchNode *successorNode(int key) const;
    BinarySearchNode *predecessorNode(int key) const;
    BinarySearchNode *minNode() const;
    BinarySearchNode *maxNode() const;

    BinarySearchNode *m_root;
};

