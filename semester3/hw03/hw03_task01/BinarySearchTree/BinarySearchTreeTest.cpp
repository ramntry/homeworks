#include <cstdlib>
#include <algorithm>
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

void BinarySearchTreeTest::fillWithRandom(std::vector<int> &v)
{
    size_t size = v.size();
    for (size_t i = 0; i < size; ++i) {
        v[i] = rand();
    }
}

void BinarySearchTreeTest::fillFromTo(int from, int to)
{
    if (to <= from) {
        return;
    }
    int variability = (to - from) / 4;
    int border = from + variability + rand() % (2 * variability + 1);
    tree_->insert(border);
    fillFromTo(from, border);
    fillFromTo(border + 1, to);
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
    EXPECT_EQ(4UL, tree_->size());
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
    EXPECT_FALSE(tree_->empty());
    EXPECT_TRUE(tree_->erase(42));
    EXPECT_FALSE(tree_->has(42));
    EXPECT_TRUE(tree_->empty());
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
    EXPECT_EQ(2UL, tree_->size());
}

TEST_F(BinarySearchTreeTest, iteratorForEmptyTreeTest)
{
    BinarySearchTree::Iterator it = tree_->iterator();
    EXPECT_FALSE(it.hasNext());
}

TEST_F(BinarySearchTreeTest, iterateWithoutChangingTest)
{
    std::vector<int> test(1000);
    fillWithRandom(test);

    tree_->insert(&test[0], test.size());
    sort(test.begin(), test.end());

    BinarySearchTree::Iterator it = tree_->iterator();
    size_t i = 0;
    for (; it.hasNext(); it.next()) {
        EXPECT_EQ(test[i++], it.value());
    }
    EXPECT_EQ(test.size(), i);
}

TEST_F(BinarySearchTreeTest, iterateWithRemoveSimpleTest)
{
    std::vector<int> test = { 2, 1, 3 };
    tree_->insert(&test[0], test.size());
    BinarySearchTree::Iterator it = tree_->iterator();
    tree_->erase(2);
    for (int i = 1; it.hasNext(); it.next()) {
        EXPECT_EQ(test[i++], it.value());
    }
}

TEST_F(BinarySearchTreeTest, iterateWithRemoveTest)
{
    size_t const testSize = 1000;
    std::vector<int> test(testSize);
    fillWithRandom(test);
    tree_->insert(&test[0], test.size());
    sort(test.begin(), test.end());

    BinarySearchTree::Iterator it = tree_->iterator();
    int k = testSize / 4;
    for (int i = 0; i < 2 * k; ++i) {
        it.next();
    }

    for (int i = 0; i < k; ++i) {
        tree_->erase(test[i]);
    }
    for (int i = 2 * k; it.hasNext(); it.next()) {
        EXPECT_EQ(test[i++], it.value());
    }
}

TEST_F(BinarySearchTreeTest, iterateWithInsertTest)
{
    size_t const testSize = 1000;  // must be even
    std::vector<int> test(testSize);
    fillWithRandom(test);
    tree_->insert(&test[0], test.size() / 2);

    BinarySearchTree::Iterator it = tree_->iterator();
    for (size_t i = 0; i < it.hasNext() && test.size() / 4; ++i) {
        it.next();
    }

    tree_->insert(&test[test.size() / 2], test.size() / 2);
    sort(test.begin(), test.end());
    std::vector<int>::iterator expectedIt = lower_bound(test.begin(), test.end(), it.value());

    for (; it.hasNext(); it.next()) {
        EXPECT_EQ(*expectedIt++, it.value());
    }
}

TEST_F(BinarySearchTreeTest, removePointeeByIteratorTest)
{
    tree_->insert(2);
    tree_->insert(1);
    tree_->insert(3);
    BinarySearchTree::Iterator it1 = tree_->iterator();
    BinarySearchTree::Iterator it2 = tree_->iterator();
    it1.next();
    tree_->erase(2);
    it2.next();
    EXPECT_EQ(2, it1.value());
    EXPECT_EQ(3, it2.value());
}

TEST_F(BinarySearchTreeTest, iterateComplexTest)
{
    size_t const testSize = 20000;
    fillFromTo(0, testSize);

    BinarySearchTree::Iterator it1 = tree_->iterator();
    BinarySearchTree::Iterator it2 = tree_->iterator();
    BinarySearchTree::Iterator it3 = tree_->iterator();
    for (int i = 0; i < 3000; ++i) {
        it1.next();
        it2.next();
    }
    for (int i = 0; i < 6000; ++i) {
        it3.next();
    }
    EXPECT_EQ(3000, it1.value());
    EXPECT_EQ(3000, it2.value());
    EXPECT_EQ(6000, it3.value());

    for (int i = 2000; i < 7000; ++i) {
        tree_->erase(i);
    }
    EXPECT_EQ(3000, it1.value());
    EXPECT_EQ(3000, it2.value());
    EXPECT_EQ(6000, it3.value());

    it2.next();
    it3.next();
    EXPECT_EQ(7000, it2.value());
    EXPECT_EQ(it2.value(), it3.value());

    for (int i = 2000; i < 4000; ++i) {
        tree_->insert(i);
    }

    for (int i = 3000; i < 4000; ++i) {
        EXPECT_EQ(i, it1.value());
        it1.next();
    }
    EXPECT_EQ(7000, it2.value());
    EXPECT_EQ(it2.value(), it3.value());
    EXPECT_EQ(it2.value(), it1.value());

    it1.next();
    it2.next();
    it3.next();
    EXPECT_EQ(7001, it2.value());
    EXPECT_EQ(it2.value(), it3.value());
    EXPECT_EQ(it2.value(), it1.value());
}

