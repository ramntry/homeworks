#pragma once
#include <vector>
#include <gtest/gtest.h>

class BinarySearchTree;

class BinarySearchTreeTest : public ::testing::Test
{
protected:
    void SetUp();
    void TearDown();

    void fillWithRandom(std::vector<int> &v);

    BinarySearchTree *tree_;
};

