#pragma once
#include <QtTest/QtTest>
#include "BinarySearchTree.h"

class BinarySearchTreeTest : public QObject
{
    Q_OBJECT

private slots:
    void init()
    {
        bst = new BinarySearchTree();
    }

    void testAddSimple()
    {
        bst->add(20);
        bst->add(10);
        bst->add(30);
        bst->add(25);
    }

    void testHas()
    {
        QVERIFY(!bst->has(10));
        bst->add(10);
        QVERIFY(bst->has(10));
    }

    void testDelLief()
    {
        bst->add(10);
        bst->add(20);
        bst->del(20);
        QVERIFY(!bst->has(20));
        QVERIFY(bst->has(10));
    }

    void testDelNodeWithOneChild()
    {
        bst->add(10);
        bst->add(20);
        bst->add(30);
        bst->del(20);

        QVERIFY(!bst->has(20));
        QVERIFY(bst->has(10));
        QVERIFY(bst->has(30));
    }

    void testDelNodeWithTwoChildAndRoot()
    {
        int toAdd[] = { 30, 20, 40, 10, 25, 23, 27, 5, 15 };
        multipleAdd(toAdd, sizeof(toAdd)/sizeof(int));

        QVERIFY(bst->has(20));
        bst->del(20);
        QVERIFY(!bst->has(20));

        QVERIFY(bst->has(30));
        bst->del(30);
        QVERIFY(!bst->has(30));

        int toHas[] = { 40, 10, 25, 23, 27, 5, 15 };
        QVERIFY(multipleHas(toHas, sizeof(toHas)/sizeof(int)));
    }

    void cleanup()
    {
        delete bst;
    }

private:
    BinarySearchTree *bst;

    void multipleAdd(int const* a, size_t size)
    {
        for (size_t i = 0; i < size; ++i)
            bst->add(a[i]);
    }

    bool multipleHas(int const* a, size_t size)
    {
        bool ok = true;

        for (size_t i = 0; i < size; ++i)
            ok = bst->has(a[i]) && ok;

        return ok;
    }
};

QTEST_MAIN(BinarySearchTreeTest)
