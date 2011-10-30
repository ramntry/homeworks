#pragma once

class BinarySearchTree
{
public:
    BinarySearchTree();
    explicit BinarySearchTree(int key);
    BinarySearchTree(const int *array, int size);

    bool isEmpty() const { return this == m_leftChild; }
    BinarySearchTree *search(int key) const;
    BinarySearchTree *getParent(int key) const;
    int min() const;
    int max() const;
    BinarySearchTree *successor(int key) const;
    BinarySearchTree *predecessor(int key) const;

    void symorder(void (*act)(int key));

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
};
