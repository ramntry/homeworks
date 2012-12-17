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
        return true;
    }
    return false;
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
    return true;
}
