#pragma once
#include <sstream>
#include <string>
#include <QtTest/QtTest>
#include "Set.h"

class SetTest : public QObject
{
    Q_OBJECT

private slots:
    void init()
    {
        s1 = new Set<int>();
        s2 = new Set<int>();
    }

    void testSetIntersection()
    {
        int a1[] = { 2, 4, 6, 8, 10, 12 };
        multipleAdd(s1, a1, 6);

        int a2[] = { 3, 6, 9, 12, 15 };
        multipleAdd(s2, a2, 5);

        Set<int> res = s1->setIntersection(*s2);
        std::ostringstream setOut;
        res.print(setOut);

        QCOMPARE(setOut.str(), std::string("6 12 "));
    }

    void testSetUnion()
    {
        int a1[] = { 2, 4, 6 };
        multipleAdd(s1, a1, 3);

        int a2[] = { 3, 6 };
        multipleAdd(s2, a2, 2);

        Set<int> res = s1->setUnion(*s2);
        std::ostringstream setOut;
        res.print(setOut);

        QCOMPARE(setOut.str(), std::string("2 3 4 6 "));
    }

    void cleanup()
    {
        delete s1;
        delete s2;
    }

private:
    Set<int> *s1;
    Set<int> *s2;

    void multipleAdd(Set<int> *s, int const* a, size_t size)
    {
        for (size_t i = 0; i < size; ++i)
            s->add(a[i]);
    }
};
