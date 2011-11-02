#pragma once
#include "binarysearchtree.h"

enum Color { Red, Black };

class RBTree// : public BinarySearchTree
{
public:
    void insert(int value);
    void remove(int value);

private:
    Color m_color;
    void rightRotate();
    void leftRotate();
};
