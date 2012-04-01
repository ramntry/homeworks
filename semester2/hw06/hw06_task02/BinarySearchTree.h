#pragma once
#include <cstdlib>
#include <cstdio>
#include <iostream>

struct TreeNode
{
    int value;
    TreeNode *leftChild;
    TreeNode *rightChild;

    TreeNode(int newValue):
        value(newValue),
        leftChild(NULL),
        rightChild(NULL)
    {}

    void printTree();
    void printTreeReverse();
    void eraseTree();
};

class BinarySearchTree
{
public:
    BinarySearchTree() : tree(NULL) {}
    ~BinarySearchTree() { if (tree) tree->eraseTree(); }

    void add(int number);
    bool has(int number) { return (bool) *getJointPoint(number); }
    void del(int number, TreeNode **start = NULL);
    void print(bool reverse = false);

private:
    // Метод, возвращающий "точку присоединения" элемента - указатель на поле в памяти, который согласно
    // ... структуре дерева должен содержать указатель на узел дерева с этим элементом в качестве значения.
    TreeNode **getJointPoint(int number, TreeNode **start = NULL);

    TreeNode *tree;
};

/*** Реализация функциональности, не требующей удаления элементов дерева ***/

TreeNode **BinarySearchTree::getJointPoint(int number, TreeNode **start)
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

void BinarySearchTree::add(int number)
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

void BinarySearchTree::print(bool reverse)
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

void BinarySearchTree::del(int number, TreeNode **start)
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

