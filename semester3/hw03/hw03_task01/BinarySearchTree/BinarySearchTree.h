#pragma once
#include <cstddef>

class BinarySearchTree
{
public:
    BinarySearchTree();
    ~BinarySearchTree();

    std::size_t size() const;
    bool empty() const;

    bool has(int value);
    bool insert(int value);
    bool erase(int value);

private:
    struct Node;

    Node **search(int value);
    void eraseFrom(Node **cursor);

    std::size_t size_;
    Node *root_;
};

