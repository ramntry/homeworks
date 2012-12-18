#include <iostream>
#include <algorithm>
#include "BinarySearchTree.h"

struct BinarySearchTree::Node
{
    int value;
    Node *left;
    Node *right;

    Node(int v, Node *l = NULL, Node *r = NULL);
    ~Node();
};

BinarySearchTree::Node::Node(int v, Node *l, Node *r)
    : value(v)
    , left(l)
    , right(r)
{
}

BinarySearchTree::Node::~Node()
{
    delete left;
    delete right;
}

BinarySearchTree::BinarySearchTree()
    : size_(0)
    , root_(NULL)
    , modcounter_(0)
{
}

BinarySearchTree::~BinarySearchTree()
{
    delete root_;
}

std::size_t BinarySearchTree::size() const
{
    return size_;
}

bool BinarySearchTree::empty() const
{
    return size() == 0;
}

BinarySearchTree::Node **BinarySearchTree::search(int value)
{
    Node **result = &root_;
    while (*result && value != (*result)->value) {
        if (value < (*result)->value) {
            result = &((*result)->left);
        } else {
            result = &((*result)->right);
        }
    }
    return result;
}

bool BinarySearchTree::has(int value)
{
    return *search(value) != NULL;
}

bool BinarySearchTree::insert(int value)
{
    Node **cursor = search(value);
    if (*cursor == NULL) {
        *cursor = new Node(value);
        ++modcounter_;
        ++size_;
        return true;
    }
    return false;
}

void BinarySearchTree::insert(int *array, size_t size)
{
    for (size_t i = 0; i < size; ++i) {
        insert(array[i]);
    }
}

void BinarySearchTree::eraseFrom(Node **cursor)
{
    Node *toDelete = *cursor;

    if (toDelete->left == NULL) {
        *cursor = toDelete->right;
        toDelete->right = NULL;
    } else if (toDelete->right == NULL) { 
        *cursor = toDelete->left;
        toDelete->left = NULL;
    } else {
        cursor = &(toDelete->right);
        while ((*cursor)->left) {
            cursor = &((*cursor)->left);
        }
        std::swap((*cursor)->value, toDelete->value);
        eraseFrom(cursor);
        toDelete = NULL;
    }

    delete toDelete;
}

bool BinarySearchTree::erase(int value)
{
    Node **cursor = search(value);
    if (*cursor == NULL) {
        return false;
    }
    eraseFrom(cursor);
    ++modcounter_;
    --size_;
    return true;
}

BinarySearchTree::Iterator::Iterator(BinarySearchTree const *bst)
    : bst_(bst)
    , modcounter_(bst->modcounter_)
{
    if (bst_->root_) {
        path_.push(bst_->root_);
        fallToLeft();
    }
}

void BinarySearchTree::Iterator::fallToLeft()
{
    Node *currentNode = path_.top()->left;
    while (currentNode) {
        path_.push(currentNode);
        currentNode = currentNode->left;
    }
}

BinarySearchTree::Iterator BinarySearchTree::iterator() const
{
    return Iterator(this);
}

int BinarySearchTree::Iterator::value() const
{
    return path_.top()->value;
}

bool BinarySearchTree::Iterator::hasNext() const
{
    return !path_.empty();
}

void BinarySearchTree::Iterator::next()
{
    Node *top = path_.top();
    if (top->right) {
        path_.push(top->right);
        fallToLeft();
    } else {
        path_.pop();
        while (!path_.empty() && path_.top()->right == top) {
            top = path_.top();
            path_.pop();
        }
    }
}

bool BinarySearchTree::Iterator::checkConsistency()
{
    return true;
}

