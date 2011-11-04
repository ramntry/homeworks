#include "binarysearchtree.h"
#include <stdexcept>

BinarySearchTree::BinarySearchTree(const int *array, int size) :
    m_root(NULL)
{
    for (int i = 0; i < size; i++)
        insert(array[i]);
}

BinarySearchNode::~BinarySearchNode()
{
    if (m_leftChild)
        delete m_leftChild;
    if (m_rightChild)
        delete m_rightChild;
}

BinarySearchTree::~BinarySearchTree()
{
    if (m_root)
        delete m_root;
}

BinarySearchResult BinarySearchTree::search(int key)
{
    BinarySearchNode **current = &m_root;    // "Бегунок"
    BinarySearchNode *parent = m_root;       // подтягивающийся за "бегунком" родитель
    while (*current && (*current)->m_key != key)
        if (key > (*current)->m_key)
        {
            parent = *current;
            current = &((*current)->m_rightChild);
        }
        else
        {
            parent = *current;
            current = &((*current)->m_leftChild);
        }
    return BinarySearchResult(*current, parent);
}

void BinarySearchTree::insert(int key)
{
    BinarySearchResult position = search(key);
    if (position == NULL)
        position.m_node = new BinarySearchNode(key);
}

void BinarySearchTree::remove(int key)
{
    BinarySearchResult place = search(key);
    if (place == NULL)                       // Если ключа нет - ничего не делать
        return;
    if (!place.m_node->m_rightChild)         // Если правого сына нет задача решается однозначно вне зависимости от
    {                                        // ... того, есть ли левый сын
        BinarySearchNode *tmp = place.m_node;
        place.m_node = place.m_node->m_leftChild;
        tmp->m_leftChild = NULL;
        tmp->m_rightChild = NULL;
        delete tmp;
    }
    else
    {
        if (place.m_node->m_rightChild->m_leftChild)  // Только так minNode отрабатывает нормально
        {
            BinarySearchResult next = BinarySearchTree(place.m_node->m_rightChild).minNode(); // шаг вправо - до
            place.m_node->m_key = next.m_node->m_key;                                         // ... упора влево
            BinarySearchTree(next.m_parent).remove(key);  // только стартуя от родителя - иначе удаление
        }                                                 // ... не отрабатывает
        else
        {
            BinarySearchNode *tmp = place.m_node;         // иначе удаляем вручную
            place.m_node = place.m_node->m_rightChild;
            tmp->m_leftChild = NULL;
            tmp->m_rightChild = NULL;
            delete tmp;
        }
    }
}

BinarySearchResult BinarySearchTree::minNode()
{
    BinarySearchNode **current = &m_root;
    BinarySearchNode *parent = m_root;
    while ((*current)->m_leftChild)
    {
        parent = *current;
        current = &((*current)->m_leftChild);
    }
    return BinarySearchResult(*current, parent);
}

BinarySearchResult BinarySearchTree::maxNode()
{
    BinarySearchNode **current = &m_root;
    BinarySearchNode *parent = m_root;
    while ((*current)->m_rightChild)
    {
        parent = *current;
        current = &((*current)->m_rightChild);
    }
    return BinarySearchResult(*current, parent);
}

void BinarySearchTree::symorder(void (*act)(int key))
{
    if (isEmpty())
        return;
    if (m_root->m_leftChild)
        BinarySearchTree(m_root->m_leftChild).symorder(act);
    act(m_root->m_key);
    if (m_root->m_rightChild)
        BinarySearchTree(m_root->m_rightChild).symorder(act);
}

void BinarySearchTree::symorderBack(void (*act)(int key))
{
    if (isEmpty())
        return;
    if (m_root->m_rightChild)
        BinarySearchTree(m_root->m_rightChild).symorderBack(act);
    act(m_root->m_key);
    if (m_root->m_leftChild)
        BinarySearchTree(m_root->m_leftChild).symorderBack(act);
}
