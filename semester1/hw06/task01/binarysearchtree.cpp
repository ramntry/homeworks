#include "binarysearchtree.h"
#include <stdexcept>

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

BinarySearchTree::BinarySearchTree(const int *array, int size) :
    m_key(*array),
    m_leftChild(0),
    m_rightChild(0)
{
    for (int i = 1; i < size; i++)
        insert(array[i]);
}

BinarySearchTree *&BinarySearchTree::step(BinarySearchTree *node, int key) const
/**
 * Принимает решение по ключу, в какую сторону делать шаг в поиске. Возвращает
 * ссылку на указатель на нужный узел - он становится доступным на изменение
 */
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
    if (isEmpty())
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
    if (isEmpty())
    {
        m_key = key;
        m_leftChild = 0;
        return;
    }
    BinarySearchTree *pos = getParent(key);
    if (pos)                                 // Если добавлямый элемент не в корне дерева
    {
        BinarySearchTree *&child = step(pos, key);
        if (!child)                          // ... и не в соответсвующем узле
            child = new BinarySearchTree(key);
    }
}

BinarySearchTree *BinarySearchTree::minNode() const
{
    if (isEmpty())
        throw std::runtime_error("Error in min(): BinarySearchTree is empty");
    BinarySearchTree *current = const_cast<BinarySearchTree *>(this);
    while (current->m_leftChild)
        current = current->m_leftChild;
    return current;
}

BinarySearchTree *BinarySearchTree::maxNode() const
{
    if (isEmpty())
        throw std::runtime_error("Error in max(): BinarySearchTree is empty");
    BinarySearchTree *current = const_cast<BinarySearchTree *>(this);
    while (current->m_rightChild)
        current = current->m_rightChild;
    return current;
}


void BinarySearchTree::symorder(void (*act)(int key))
{
    if (isEmpty())
        return;
    if (m_leftChild)
        m_leftChild->symorder(act);
    act(m_key);
    if (m_rightChild)
        m_rightChild->symorder(act);
}

BinarySearchTree *BinarySearchTree::successorNode(int key) const
{
    if (isEmpty())
        return NULL;

    BinarySearchTree *current = const_cast<BinarySearchTree *>(this);
    BinarySearchTree *ans = NULL;
    while (current && current->m_key != key)
        if (key < current->m_key)
        {
            ans = current;                    // Родителя, от которого был последний поворот налево запоминаем -
            current = current->m_leftChild;   // ... если у узла с ключом key не окажется правого сына ключ такого
        }                                     // ... родителя и окажется искомым ключом, следующим за key
        else
            current = current->m_rightChild;
    if (!current->m_rightChild)
        return ans;
    return current->m_rightChild->minNode();
}

//void BinarySearchTree::remove(BinarySearchTree *subtree)
//{
//}

void BinarySearchTree::remove(int key)
{
    BinarySearchTree *parent = getParent(key);
    BinarySearchTree *&toRemove = step(parent, key);
    if (!(toRemove->m_leftChild && toRemove->m_rightChild))
    {
        BinarySearchTree *tmp = toRemove;
        toRemove = toRemove->m_rightChild ? toRemove->m_rightChild : toRemove->m_leftChild;
        delete tmp;
    }
    else
    {
        BinarySearchTree *tmp = toRemove->m_rightChild->minNode();
        toRemove->m_key = tmp->m_key;
        toRemove->m_rightChild->remove(tmp->m_key);
    }
}
