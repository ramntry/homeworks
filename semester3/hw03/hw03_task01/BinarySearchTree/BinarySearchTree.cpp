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

BinarySearchTree::Node **BinarySearchTree::search(int value
        , std::vector<Node *> *path)
{
    Node **result = &root_;
    while (*result && value != (*result)->value) {
        if (path) {
            path->push_back(*result);
        }
        if (value < (*result)->value) {
            result = &((*result)->left);
        } else {
            result = &((*result)->right);
        }
    }
    if (path) {
        path->push_back(*result);
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

BinarySearchTree::Iterator::Iterator(BinarySearchTree *bst)
    : bst_(bst)
    , modcounter_(bst->modcounter_)
{
    if (bst_->root_) {
        path_.push_back(bst_->root_);
        fallToLeft();
        value_ = path_.back()->value;
    }
}

void BinarySearchTree::Iterator::fallToLeft()
{
    Node *currentNode = path_.back()->left;
    while (currentNode) {
        path_.push_back(currentNode);
        currentNode = currentNode->left;
    }
}

BinarySearchTree::Iterator BinarySearchTree::iterator()
{
    return Iterator(this);
}

int BinarySearchTree::Iterator::value()
{
    return value_;
}

bool BinarySearchTree::Iterator::hasNext()
{
    return refreshValueIfPossible();
}

bool BinarySearchTree::Iterator::refreshValueIfPossible()
{
    if(path_.empty()) {
        return false;
    }
    value_ = path_.back()->value;
    return true;
}

void BinarySearchTree::Iterator::next()
{
    checkConsistency();
    Node *top = path_.back();
    if (top->right) {
        path_.push_back(top->right);
        fallToLeft();
    } else {
        path_.pop_back();
        while (!path_.empty() && path_.back()->right == top) {
            top = path_.back();
            path_.pop_back();
        }
    }
    refreshValueIfPossible();
}

void BinarySearchTree::Iterator::checkConsistency()
{
    if (bst_->modcounter_ == modcounter_) {
        return;
    }
    modcounter_ = bst_->modcounter_;
    path_.clear();
    bst_->search(value_, &path_);

    if (path_.back() == NULL) {
        path_.pop_back();
    }
}

