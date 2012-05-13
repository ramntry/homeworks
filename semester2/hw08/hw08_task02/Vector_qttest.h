#pragma once
#include <iostream>
#include <sstream>
#include <vector>
#include <QtTest/QtTest>
#include "Vector.h"

class VectorTest : public QObject
{
    Q_OBJECT

private slots:
    void init()
    {
        v = new Vector<int, 5>(42);
    }

    void OutToStreamTest()
    {
        std::ostringstream out;
        out << *v;
        QCOMPARE(out.str(), std::string("Vector[5] ( 42 0 0 0 0 )"));
    }

    void ConstructFromCollectionAndEqualTest()
    {
        std::vector<int> init(3);
        for (int i = 1; i < 4; ++i)
            init[i - 1] = 10 * i;

        Vector<int, 5> v2(init.begin(), init.end());
        (*v)[0] = 10;
        (*v)[1] = 20;
        (*v)[2] = 30;

        QVERIFY(v->isEqual(v2));
    }

    void mathTest()
    {
        int a2[4] = { -42, 10,  20,  30    };
        Vector<int, 5> v2(a2, a2 + 4);

        int a3[5] = {  21, -5, -10, -15, 100};
        Vector<int, 5> v3(a3, a3 + 5);

        Vector<int, 5> v4;
        v4[4] = 200;

        Vector<int, 5> tmp = v->add(v2).add(v3.scalarMul(2));
        QVERIFY(v->isEqual(tmp.sub(v4)));
    }

    void scalarAndNullTest()
    {
        QVERIFY(v->isScalar() && !v->isNull());

        (*v)[0] = 0;
        (*v)[4] = 1;
        QVERIFY(!v->isScalar() && !v->isNull());

        (*v)[4] = 0;
        QVERIFY(v->isScalar() && v->isNull());
    }

    void cleanup()
    {
        delete v;
    }

private:
    Vector<int, 5> *v;
};
