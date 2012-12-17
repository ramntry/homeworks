#include "BinarySearchTree.h"
#include "BinarySearchTreeTest.h"

void BinarySearchTreeTest::SetUp()
{
    tree_ = new BinarySearchTree;
}

void BinarySearchTreeTest::TearDown()
{
    delete tree_;
}

