#pragma once
#include <cstdlib>

class BinarySearchTree
{
public:
    BinarySearchTree();
    explicit BinarySearchTree(int key);
    BinarySearchTree(const int *array, int size);

    bool isEmpty() const { return this == m_leftChild; }
    BinarySearchTree *search(int key) const;
    BinarySearchTree *getParent(int key) const;
    BinarySearchTree *successorNode(int key) const;
    int successor(int key) const { return successorNode(key)->m_key; }

    BinarySearchTree *predecessor(int key) const;
    BinarySearchTree *minNode() const;
    BinarySearchTree *maxNode() const;
    int min() const { return minNode()->m_key; }
    int max() const { return maxNode()->m_key; }

    void symorder(void (*act)(int key));

//    virtual void insert(BinarySearchTree *subtree);
    virtual void insert(int key);
//    virtual void remove(BinarySearchTree *subtree);
    virtual void remove(int key);

protected:
    BinarySearchTree *&step(BinarySearchTree *node, int key) const;

private:
    int m_key;
    BinarySearchTree *m_leftChild;
    BinarySearchTree *m_rightChild;
};
