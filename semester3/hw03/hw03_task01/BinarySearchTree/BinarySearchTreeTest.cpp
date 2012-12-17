#include <cstdlib>
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

TEST_F(BinarySearchTreeTest, justCreateEmptyAndSizeMethodsTest)
{
    EXPECT_EQ(0UL, tree_->size());
    EXPECT_TRUE(tree_->empty());
}

TEST_F(BinarySearchTreeTest, hasForEmptyTreeTest)
{
    EXPECT_FALSE(tree_->has(rand()));
    EXPECT_FALSE(tree_->has(rand()));
}

TEST_F(BinarySearchTreeTest, insertAndHasTest)
{
    EXPECT_TRUE(tree_->insert(42));
    EXPECT_TRUE(tree_->has(42));
    EXPECT_FALSE(tree_->insert(42));
    EXPECT_TRUE(tree_->has(42));

    EXPECT_TRUE(tree_->insert(20));
    EXPECT_TRUE(tree_->insert(50));
    EXPECT_TRUE(tree_->insert(60));
    EXPECT_TRUE(tree_->has(20));
    EXPECT_TRUE(tree_->has(50));
    EXPECT_TRUE(tree_->has(60));
}

TEST_F(BinarySearchTreeTest, eraseFromEmptyTreeTest)
{
    EXPECT_FALSE(tree_->erase(rand()));
}

TEST_F(BinarySearchTreeTest, eraseUnxistingElementTest)
{
    tree_->insert(30);
    tree_->insert(20);
    tree_->insert(40);
    EXPECT_FALSE(tree_->erase(42));
}

TEST_F(BinarySearchTreeTest, eraseLeafTest)
{
    tree_->insert(42);
    EXPECT_TRUE(tree_->erase(42));
    EXPECT_FALSE(tree_->has(42));
}

TEST_F(BinarySearchTreeTest, eraseListNodeTest)
{
    tree_->insert(20);
    tree_->insert(40);
    EXPECT_TRUE(tree_->erase(20));
    EXPECT_FALSE(tree_->has(20));
    EXPECT_TRUE(tree_->has(40));
}

TEST_F(BinarySearchTreeTest, eraseTreeNodeTest)
{
    tree_->insert(30);
    tree_->insert(20);
    tree_->insert(40);
    EXPECT_TRUE(tree_->erase(30));
    EXPECT_FALSE(tree_->has(30));
    EXPECT_TRUE(tree_->has(20));
    EXPECT_TRUE(tree_->has(40));
}

