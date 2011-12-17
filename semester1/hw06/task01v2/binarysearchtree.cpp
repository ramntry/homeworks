#include "binarysearchtree.h"
#include <cstdio>

/*** Реализация функциональности, не требующей удаления элементов дерева ***/

TreeNode **NumbersSet::getJointPoint(int number, TreeNode **start)
{
    TreeNode **current = start ? start : &tree;
    while (*current != NULL && number != (*current)->value)
    {
        if (number < (*current)->value)
            current = &(*current)->leftChild;
        else
            current = &(*current)->rightChild;
    }
    return current;
}

void NumbersSet::add(int number)
{
    TreeNode **joinPoint = getJointPoint(number);
    if (!(*joinPoint))  // если указуемое точкой присоединения поле пусто, привяжем к нему новый узел
        *joinPoint = new TreeNode(number);
}

/*** Обходы дерева: симметрический, обратный симметрический, postorder ***/

void TreeNode::printTree()
{
    if (leftChild != NULL)
        leftChild->printTree();

    printf("%d ", value);

    if (rightChild != NULL)
        rightChild->printTree();
}

void TreeNode::printTreeReverse()
{
    if (rightChild != NULL)
        rightChild->printTreeReverse();

    printf("%d ", value);

    if (leftChild != NULL)
        leftChild->printTreeReverse();
}

void TreeNode::eraseTree()
{
    if (leftChild != NULL)
        leftChild->eraseTree();

    if (rightChild != NULL)
        rightChild->eraseTree();

    delete this;
}

void NumbersSet::print(bool reverse)
{
    if (tree == NULL)
        return;

    if (!reverse)
        tree->printTree();
    else
        tree->printTreeReverse();

    putchar('\n');
}

/*** удаление элемента из дерева ***/

void NumbersSet::del(int number, TreeNode **start)
{
    TreeNode **current = getJointPoint(number, start);
    if (!*current)
        return;

    if ((*current)->leftChild != NULL && (*current)->rightChild != NULL) // если есть оба сына
    {
        TreeNode **toSwap = &(*current)->rightChild;  // ... найдем наименьший элемент в правом поддереве
        while ((*toSwap)->leftChild)
            toSwap = &(*toSwap)->leftChild;

        (*current)->value = (*toSwap)->value;  // ... запишем его на место удаляемого
        del((*toSwap)->value, toSwap);         // ... и удалим рекурсивно
    }
    else
    {   // если не оба сына есть, то выберем существующий (или NULL, если нет обоих)
        TreeNode *tmp = (*current)->leftChild ? (*current)->leftChild : (*current)->rightChild;
        delete *current;  // ... и вырежем удаляемый узел, восстановив необходимые связи
        *current = tmp;
    }
}
