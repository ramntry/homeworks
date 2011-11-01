#pragma once
#include <cstdlib>

class BinarySearchTree
{
public:
    BinarySearchTree();
    BinarySearchTree(const int *array, int size);

    bool isEmpty() const { return m_parent == NULL; }
    bool hasKey(int key) const { return search(key) != NULL; }
    int successor(int key) const { return successorNode(key)->m_key; }
    int predecessor(int key) const { return predecessorNode(key)->m_key; }
    int min() const { return minNode()->m_key; }
    int max() const { return maxNode()->m_key; }
    virtual void insert(int key);
    virtual void remove(int key);

protected:
    BinarySearchTree(BinarySearchTree *parent, int key);
    BinarySearchTree *search(int key) const;
    BinarySearchTree *successorNode(int key) const;
    BinarySearchTree *predecessorNode(int key) const;
    BinarySearchTree *minNode() const;
    BinarySearchTree *maxNode() const;

    void symorder(void (*act)(int key));

    int m_key;
    BinarySearchTree *m_parent;
    BinarySearchTree *m_leftChild;
    BinarySearchTree *m_rightChild;
};
