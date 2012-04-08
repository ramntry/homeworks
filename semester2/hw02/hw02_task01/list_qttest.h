#pragma once
#include <QtTest/QtTest>
#include "arraylist.h"
#include "linkedlist.h"

#define TEST(testName)                          \
    void ListTest::testName()                   \
    {                                           \
        for (int i = 0; i < numOfClasses; ++i)  \
            testName(lists[i]);                 \
    }                                           \
    void ListTest::testName(List *l)

class ListTest : public QObject
{
    Q_OBJECT

private slots:
    void init()
    {
        lists[0] = new ArrayList;
        lists[1] = new LinkedList;
    }

    void cleanup();
    void cleanup(List *l);

    void testLengthOfEmptyList();
    void testLengthOfEmptyList(List *l);

    void testSimpleInsert();
    void testSimpleInsert(List *l);

    void testAt();
    void testAt(List *l);

    void testCompositeInsert();
    void testCompositeInsert(List *l);

    void testSimpleRemove();
    void testSimpleRemove(List *l);

    void testCompositeRemove();
    void testCompositeRemove(List *l);

    void testSimpleFind();
    void testSimpleFind(List *l);

    void testCompositeFind();
    void testCompositeFind(List *l);


private:
    static const int numOfClasses = 2;
    List *lists[numOfClasses];

    bool multipleAt(List *l, int *a, size_t size)
    {
        bool ok = true;
        for (size_t i = 0; i < size; ++i)
            ok = ok && (l->at(i) == a[i]);

        return ok;
    }

    void multipleInsert(List *l, int *i, int *v, size_t size)
    {
        for (size_t k = 0; k < size; ++k)
            l->insert(i[k], v[k]);
    }
};

TEST(cleanup)
{
    delete l;
}

TEST(testLengthOfEmptyList)
{
    QCOMPARE(l->length(), 0);
}

TEST(testSimpleInsert)
{
    l->insert(0, 100);
    QCOMPARE(l->length(), 1);
}

TEST(testAt)
{
    l->insert(0, 200);
    QCOMPARE(l->at(0), 200);
}

TEST(testCompositeInsert)
{
    int i[] = { 0, 0, 0, 1, 3, 5 };
    int v[] = { 1, 2, 3, 4, 5 ,6 };
    multipleInsert(l, i, v, sizeof(i)/sizeof(int));

    int a[] = { 3, 4, 2, 5, 1, 6 };
    QVERIFY(multipleAt(l, a, sizeof(a)/sizeof(int)));
}

TEST(testSimpleRemove)
{
    l->insert(0, 10);
    l->remove(0);
    QCOMPARE(l->length(), 0);
}

TEST(testCompositeRemove)
{
    int i[] = { 0, 0, 0, 1, 3, 5, 6, 7, 8 };
    int v[] = { 1, 2, 3, 4, 5 ,6, 7, 8, 9 };
    multipleInsert(l, i, v, sizeof(i)/sizeof(int));

    l->remove(0);
    l->remove(7);
    l->remove(1);
    l->remove(4);
    l->remove(2);

    int a[] = { 4, 5, 6, 8 };
    QVERIFY(multipleAt(l, a, sizeof(a)/sizeof(int)));
}

TEST(testSimpleFind)
{
    l->insert(0, 300);
    QCOMPARE(l->find(300), 0);
}

TEST(testCompositeFind)
{
    int i[]    = { 0, 0, 0, 1, 3, 5, 6, 7, 8 };
    int v[]    = { 1, 2, 3, 4, 5 ,6, 7, 8, 9 };
    multipleInsert(l, i, v, sizeof(i)/sizeof(int));

    int to_f[] = { 3, 9, 4, 8, 1, 10 };
    int    f[] = { 0, 8, 1, 7, 4, -1 };

    for (size_t i = 0; i < sizeof(to_f)/sizeof(int); ++i)
        QCOMPARE(l->find(to_f[i]), f[i]);
}

QTEST_APPLESS_MAIN(ListTest)
