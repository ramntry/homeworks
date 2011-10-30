#pragma once
#include "binarysearchtree.h"

enum Color { Red, Black };

class RBTree : public BinarySearchTree
{
public:
    RBTree();

    void insert(BinarySearchTree *subtree);
    void insert(int value);
    void remove(BinarySearchTree *subtree);
    void remove(int value);

private:
    Color m_color;
};
