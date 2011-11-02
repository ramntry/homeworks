#include "binarysearchtree.h"
#include <stdexcept>

BinarySearchTree::BinarySearchTree(const int *array, int size) :
    m_root(NULL)
{
    for (int i = 0; i < size; i++)
        insert(array[i]);
}

BinarySearchResult BinarySearchTree::search(int key)
{
    BinarySearchNode **current = &m_root;
    BinarySearchNode *parent = m_root;
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
    if (place == NULL)
        return;
    if (!place.m_node->m_rightChild)
    {
        BinarySearchNode *tmp = place.m_node;
        place.m_node = place.m_node->m_leftChild;
        delete tmp;
    }
    else
    {
        if (place.m_node->m_rightChild->m_leftChild)
        {
            BinarySearchResult next = BinarySearchTree(place.m_node->m_rightChild).minNode();
            place.m_node->m_key = next.m_node->m_key;
            BinarySearchTree(next.m_parent).remove(key);
        }
        else
        {
            BinarySearchNode *tmp = place.m_node;
            place.m_node = place.m_node->m_rightChild;
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
