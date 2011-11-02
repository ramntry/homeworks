#pragma once
#include <cstdlib>

struct BinarySearchNode
{
    int m_key;
    BinarySearchNode *m_leftChild;
    BinarySearchNode *m_rightChild;

    BinarySearchNode(int key) :
        m_key(key), m_leftChild(NULL), m_rightChild(NULL) {}
};

struct BinarySearchResult
{
    BinarySearchNode *&m_node;
    BinarySearchNode *m_parent;

    BinarySearchResult(BinarySearchNode *&place, BinarySearchNode *parent) :
        m_node(place), m_parent(parent) {}

    bool operator ==(BinarySearchNode *right) const { return right == m_node; }
    bool operator !=(BinarySearchNode *right) const { return right != m_node; }
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
    int min() { return minNode().m_node->m_key; }
    int max() { return maxNode().m_node->m_key; }

    virtual void insert(int key);
    virtual void remove(int key);

    void symorder(void (*act)(int key));
    void symorderBack(void (*act)(int key));

protected:
    BinarySearchResult search(int key);

    BinarySearchNode *successorNode(int key) const;
    BinarySearchNode *predecessorNode(int key) const;
    BinarySearchResult minNode();
    BinarySearchResult maxNode();

    BinarySearchNode *m_root;
};
