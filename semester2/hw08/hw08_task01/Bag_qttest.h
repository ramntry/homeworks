#pragma once
#include <QtTest/QtTest>
#include <QtCore/QTime>
#include "sstream"
#include "Bag.h"

class BagTest : public QObject
{
    Q_OBJECT

private slots:
    void initTestCase()
    {
        qsrand(QTime::currentTime().msec());
    }

    void init()
    {
        mtreap = new Bag;
    }

    void sizeAndHasInEmptyTreapSimpleTest()
    {
        QCOMPARE(mtreap->size(), (size_t) 0);
        QVERIFY(!mtreap->has(0));
    }

    void uniqAddTest()
    {
        for (size_t i = 0; i < 10000; ++i)
            mtreap->add(i);

        QCOMPARE(mtreap->size(), (size_t) 10000);

        for (size_t i = 0; i < 10000; ++i)
            QVERIFY(mtreap->has(i));
    }

    void sameAddSimpleTest()
    {
        for (size_t i = 0; i < 100; ++i)
            for (size_t j = 0; j < 100; ++j)
                mtreap->add(j);

        QCOMPARE(mtreap->size(), (size_t) 10000);

        for (size_t i = 0; i < 100; ++i)
            QVERIFY(mtreap->has(i));
    }

    void removeFromEmptyTreapAndNonExistsValuesTest()
    {
        mtreap->remove(42);
        QCOMPARE(mtreap->size(), (size_t) 0);

        mtreap->add(5);
        mtreap->add(80);
        mtreap->remove(42);
        QCOMPARE(mtreap->size(), (size_t) 2);
        QVERIFY(mtreap->has(5));
        QVERIFY(mtreap->has(80));
    }

    void uniqRemoveTest()
    {
        for (size_t i = 0; i < 10000; ++i)
            mtreap->add(i);

        for (size_t i = 0; i < 10000; ++i)
        {
            mtreap->remove(i);
            QVERIFY(!mtreap->has(i));
        }

        QCOMPARE(mtreap->size(), (size_t) 0);
    }

    void sameRemoveAndSameAddCompositeTest()
    {
        for (size_t i = 0; i < 100; ++i)
            for (size_t j = 0; j < 100; ++j)
                mtreap->add(j);

        for (size_t i = 0; i < 100; ++i)
            for (size_t j = 0; j < 50; ++j)
                mtreap->remove(j);

        QCOMPARE(mtreap->size(), (size_t) 5000);

        for (size_t i = 50; i < 100; ++i)
        {
            for (size_t j = 0; j < 100; ++j)
            {
                QVERIFY(mtreap->has(i));
                mtreap->remove(i);
            }
            QVERIFY(!mtreap->has(i));
        }

        QCOMPARE(mtreap->size(), (size_t) 0);
    }

    void eraseTest()
    {
        for (size_t i = 0; i < 100; ++i)
            for (size_t j = 0; j < 100; ++j)
                mtreap->add(j);

        for (size_t i = 0; i < 100; ++i)
        {
            QVERIFY(mtreap->has(i));
            mtreap->erase(i);
            QVERIFY(!mtreap->has(i));
        }

        QCOMPARE(mtreap->size(), (size_t) 0);
    }

    void outToStreamTest()
    {
        std::ostringstream out;
        for (int i = 0; i < 4; ++i)
        {
            mtreap->add(i);
            mtreap->add(4 - i);
        }

        out << *mtreap;
        QCOMPARE(out.str(), std::string("Bag( 0 1 1 2 2 3 3 4 )"));
    }

    void endIteratorsEqualityTest()
    {
        QVERIFY(mtreap->end() == mtreap->end());
    }

    void traversalIteratorInitTest()
    {
        for (int i = 4; i < 10; ++i)
            mtreap->add(i / 2);

        Bag::Iterator it = mtreap->begin();
        QCOMPARE(*it, 2);
    }

    void traversalIteratorEmptyAndOneElementBag()
    {
        Bag::Iterator it = mtreap->begin();
        QCOMPARE(it, mtreap->end());

        mtreap->add(42);
        Bag::Iterator it2 = mtreap->begin();
        QVERIFY(it2 != mtreap->end());
        ++it2;
        QVERIFY(it2 == mtreap->end());
    }

    void traversalIteratorFullTest()
    {
        std::ostringstream trueOut;
        for (int i = 0; i < 1000; ++i)
        {
            mtreap->add(i);
            trueOut << i << ' ';
        }

        std::ostringstream out;
        for (Bag::Iterator it = mtreap->begin(); it != mtreap->end(); ++it)
            out << *it << ' ';

        QCOMPARE(out.str(), trueOut.str());
    }

    void cleanup()
    {
        delete mtreap;
    }

private:
    Bag *mtreap;
};
