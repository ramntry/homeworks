#pragma once
#include <gtest/gtest.h>

class BinarySearchTree;

class BinarySearchTreeTest : public ::testing::Test
{
protected:
    void SetUp();
    void TearDown();

    BinarySearchTree *tree_;
};

