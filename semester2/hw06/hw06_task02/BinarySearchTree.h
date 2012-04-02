#pragma once
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include "../../hw03/hw03_task01/comparator.h"

template <typename T, typename C = StandartComparator<T> >
class BinarySearchTree
{
    struct TreeNode

    {
        T value;
        TreeNode *leftChild;
        TreeNode *rightChild;

        TreeNode(T newValue):
            value(newValue),
            leftChild(NULL),
            rightChild(NULL)
        {}
        template <typename F> void symmetric(F &functor);
        template <typename F> void reverse(F &functor);
        template <typename F> void preorder(F &functor);
        void eraseTree();
    };

    struct TreePrinter
    {
        TreePrinter(std::ostream &_os, char _delim)
            : os(_os)
            , delim(_delim)
        {}
        void operator ()(T const& value) const
        {
            os << value << delim;
        }
        std::ostream &os;
        char delim;
    };

public:
    BinarySearchTree()
        : tree(NULL)
    {}
    ~BinarySearchTree()
    {
        if (tree)
            tree->eraseTree();
    }

    void add(T item);
    bool has(T item) { return (bool) *getJoinPoint(item); }
    void del(T item, TreeNode **start = NULL);
    void print(std::ostream &os = std::cout, char delim = ' ', bool reverse = false);

protected:
    // Метод, возвращающий "точку присоединения" элемента - указатель на поле в памяти, который согласно
    // ... структуре дерева должен содержать указатель на узел дерева с этим элементом в качестве значения.
    TreeNode **getJoinPoint(T item, TreeNode **start = NULL);

    TreeNode *tree;
    C comp;
};

/*** Реализация функциональности, не требующей удаления элементов дерева ***/

template <typename T, typename C>
typename BinarySearchTree<T, C>::TreeNode **BinarySearchTree<T, C>::getJoinPoint(T item, TreeNode **start)
{
    TreeNode **current = start ? start : &tree;
    while (*current != NULL && comp(item, (*current)->value) != 0)
    {
        if (comp(item, (*current)->value) < 0)
            current = &(*current)->leftChild;
        else
            current = &(*current)->rightChild;
    }
    return current;
}


template <typename T, typename C>
void BinarySearchTree<T, C>::add(T item)
{
    TreeNode **joinPoint = getJoinPoint(item);
    if (!(*joinPoint))  // если указуемое точкой присоединения поле пусто, привяжем к нему новый узел
        *joinPoint = new TreeNode(item);
}

/*** Обходы дерева: симметрический, обратный симметрический, postorder ***/

template <typename T, typename C>
template <typename F>
void BinarySearchTree<T, C>::TreeNode::symmetric(F &functor)
{
    if (leftChild != NULL)
        leftChild->symmetric(functor);

    functor(value);

    if (rightChild != NULL)
        rightChild->symmetric(functor);
}

template <typename T, typename C>
template <typename F>
void BinarySearchTree<T, C>::TreeNode::reverse(F &functor)
{
    if (rightChild != NULL)
        rightChild->reverse(functor);

    functor(value);

    if (leftChild != NULL)
        leftChild->reverse(functor);
}

template <typename T, typename C>
template <typename F>
void BinarySearchTree<T, C>::TreeNode::preorder(F &functor)
{
    functor(value);

    if (leftChild != NULL)
        leftChild->symmetric(functor);

    if (rightChild != NULL)
        rightChild->symmetric(functor);
}

template <typename T, typename C>
void BinarySearchTree<T, C>::TreeNode::eraseTree()
{
    if (leftChild != NULL)
        leftChild->eraseTree();

    if (rightChild != NULL)
        rightChild->eraseTree();

    delete this;
}

template <typename T, typename C>
void BinarySearchTree<T, C>::print(std::ostream &os, char delim, bool reverse)
{
    if (tree == NULL)
        return;

    TreePrinter printer(os, delim);

    if (!reverse)
        tree->symmetric(printer);
    else
        tree->reverse(printer);
}

/*** удаление элемента из дерева ***/

template <typename T, typename C>
void BinarySearchTree<T, C>::del(T item, TreeNode **start)
{
    TreeNode **current = getJoinPoint(item, start);
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
