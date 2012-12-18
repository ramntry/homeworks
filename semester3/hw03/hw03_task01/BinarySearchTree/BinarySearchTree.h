#pragma once
#include <cstddef>
#include <stack>

class BinarySearchTree
{
public:
    class Iterator;

    BinarySearchTree();
    ~BinarySearchTree();

    std::size_t size() const;
    bool empty() const;

    bool has(int value);
    bool insert(int value);
    void insert(int *array, size_t size);
    bool erase(int value);

    Iterator iterator() const;

private:
    struct Node;

    Node **search(int value);
    void eraseFrom(Node **cursor);

    std::size_t size_;
    Node *root_;
    unsigned long long modcounter_;
};

class BinarySearchTree::Iterator
{
public:
    Iterator(BinarySearchTree const *bst);

    int value() const;
    bool hasNext() const;
    void next();

private:
    void fallToLeft();
    bool checkConsistency();

    BinarySearchTree const *bst_;
    std::stack<Node *> path_;
    unsigned long long modcounter_;
};

