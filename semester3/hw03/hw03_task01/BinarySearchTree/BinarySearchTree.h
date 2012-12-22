#pragma once
#include <cstddef>
#include <vector>

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

    Iterator iterator();

private:
    struct Node;

    Node **search(int value, std::vector<Node *> *path = NULL);
    void eraseFrom(Node **cursor);

    std::size_t size_;
    Node *root_;
    unsigned long long modcounter_;
};

class BinarySearchTree::Iterator
{
public:
    Iterator(BinarySearchTree *bst);

    int value();
    bool hasNext();
    void next();

private:
    void fallToLeft();
    void checkConsistency();
    bool refreshValueIfPossible();

    BinarySearchTree *bst_;
    std::vector<Node *> path_;
    int value_;
    unsigned long long modcounter_;
};

