#pragma once
#include <cstdlib>

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

class NumbersSet
{
public:
    NumbersSet() : tree(NULL) {}
    ~NumbersSet() { if (tree) tree->eraseTree(); }

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
