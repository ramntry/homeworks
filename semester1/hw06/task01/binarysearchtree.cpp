#include "binarysearchtree.h"

BinarySearchTree::BinarySearchTree() :
    m_key(0),
    m_leftChild(1),   // Указатели на смежные вершины в позиционном графе могут быть одинаковыми только в одном
    m_rightChild(1),  // ... случае: при отсутствии соответсвующих вершин они нулевые. 1 - специальное значение,
    m_parent(0)       // ... свидетельствующее о том, что дерево пока пусто.
{}

BinarySearchTree::BinarySearchTree(int key) :
    m_key(key),
    m_leftChild(0),
    m_rightChild(0),
    m_parent(0)
{}
