#include "binarysearchtree.h"

BinarySearchTree::BinarySearchTree() :
    m_key(0),
    m_leftChild(this),  // Специальный маркер пустоты дерева
    m_rightChild(0)
{}

BinarySearchTree::BinarySearchTree(int key) :
    m_key(key),
    m_leftChild(0),
    m_rightChild(0)
{}

BinarySearchTree *&BinarySearchTree::step(BinarySearchTree *node, int key) const
{
    if (key > node->m_key)
        return node->m_rightChild;
    return node->m_leftChild;
}

BinarySearchTree *BinarySearchTree::search(int key) const
/**
 * Метод возвращает указатель на узел дерева с ключом key или 0, если такого ключа в дереве нет.
 * Метод малополезен для экземпляров класса BinarySearchTree, но может представлять интерес для его наследников:
 * например, это образ метода has для множества или has_key для отображения и метода operator [] для отображения.
 */
{
    if (this == m_leftChild)                 // Если дерево пусто
        return 0;
    BinarySearchTree *res = const_cast<BinarySearchTree *>(this);
    while (res && res->m_key != key)
        res = step(res, key);
    return res;
}

BinarySearchTree *BinarySearchTree::getParent(int key) const
/**
 * Метод возвращает указатель на узел дерева, который является родителем узла со значением key или был бы им, если бы
 * такой ключ присутствовал. Возвращает 0 в случае, если ключ key является ключом корня дерева. Не обрабатывает
 * ситуацию с пустым деревом.
 */
{
    BinarySearchTree *parent = 0;
    BinarySearchTree *current = const_cast<BinarySearchTree *>(this);
    while (current && current->m_key != key)
    {
        parent = current;
        current = step(current, key);
    }
    return parent;
}

void BinarySearchTree::insert(int key)
{
    if (this == m_leftChild)                 // Если дерево пусто
    {
        m_key = key;
        m_leftChild = 0;
        return;
    }
    BinarySearchTree *pos = getParent(key);
    BinarySearchTree *&child = pos ? step(pos, key) : pos;
    if (pos && !child)                      // Если такого элемента еще нет
        child = new BinarySearchTree(key);
}
