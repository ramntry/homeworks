#pragma once

class BinarySearchTree
{
public:
    BinarySearchTree();

    BinarySearchTree *search(int value) const;
    BinarySearchTree *min() const;
    BinarySearchTree *max() const;
    BinarySearchTree *successor(int value) const;
    BinarySearchTree *predecessor(int value) const;

    virtual void insert(BinarySearchTree *subtree);
    virtual void insert(int value);
    virtual void remove(BinarySearchTree *subtree);
    virtual void remove(int value);

private:
    int m_key;
    BinarySearchTree *m_leftChild;
    BinarySearchTree *m_rigthChild;
    BinarySearchTree *m_parent;
};
