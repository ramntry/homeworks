#pragma once

class BinarySearchTree
{
public:
    BinarySearchTree();
    explicit BinarySearchTree(int key);
//    BinarySearchTree(const int *array, int size);

    BinarySearchTree *search(int key) const;
    BinarySearchTree *getParent(int key) const;
    BinarySearchTree *min() const;
    BinarySearchTree *max() const;
    BinarySearchTree *successor(int key) const;
    BinarySearchTree *predecessor(int key) const;

//    virtual void insert(BinarySearchTree *subtree);
    virtual void insert(int key);
//    virtual void remove(BinarySearchTree *subtree);
//    virtual void remove(int key);

protected:
    BinarySearchTree *&step(BinarySearchTree *node, int key) const;

private:
    int m_key;
    BinarySearchTree *m_leftChild;
    BinarySearchTree *m_rightChild;
//    BinarySearchTree *m_parent;
};
