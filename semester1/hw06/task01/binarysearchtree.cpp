#include "binarysearchtree.h"
#include <stdexcept>

BinarySearchNode::BinarySearchNode(int key) :
    m_key(key),
    m_leftChild(NULL),
    m_rightChild(NULL)
{}

BinarySearchTree::BinarySearchTree(const int *array, int size) :
    m_root(NULL)
{
    for (int i = 0; i < size; i++)
        insert(array[i]);
}

BinarySearchResult BinarySearchTree::search(int key)
/**
 * Метод возвращает ссылку на указатель на узел дерева, соответствующий ключу key вне зависимости от его наличия.
 */
{
    BinarySearchNode **current = &m_root;
    BinarySearchNode *parent = NULL;
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
/**
 * Метод осуществляет вставку нового элемента в дерево, если его еще в нем нет, с сохранением свойств дерева
 */
{
    BinarySearchResult position = search(key);
    if (position == NULL)
    {
        position.m_place = new BinarySearchNode(key);
        position.m_place->m_parent = position.m_parent;
    }
}

void BinarySearchTree::remove(int key)
{
    BinarySearchResult place = search(key);
    if (place == NULL)
        return;
    if (!place.m_place->m_rightChild)
    {
        place.m_parent->m_leftChild = place.m_place->m_leftChild;
        delete place.m_place;
    }
    else if (m_root == place.m_place)
    {
        BinarySearchNode *succ = BinarySearchTree(place.m_place->m_rightChild).minNode();
        place.m_place->m_key = succ->m_key;
        BinarySearchTree(succ).remove(key);
    }
    else
    {
        delete m_root;
        m_root = NULL;
    }
}

BinarySearchNode *BinarySearchTree::minNode() const
{
    BinarySearchNode *result = m_root;
    while (result->m_leftChild)
        result = result->m_leftChild;
    return result;
}

BinarySearchNode *BinarySearchTree::maxNode() const
{
    BinarySearchNode *result = m_root;
    while (result->m_rightChild)
        result = result->m_rightChild;
    return result;
}

/*
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

*/
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
/*
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
*/
