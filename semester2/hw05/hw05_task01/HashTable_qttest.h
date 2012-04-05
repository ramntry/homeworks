#pragma once
#include <QtCore/QString>
#include <QtTest/QtTest>
#include "HashTable.h"

class HashTableTest : public QObject
{
    Q_OBJECT

private slots:
    void init()
    {
        t = new HashTable<QString>;
    }

    void testHasherIsSetInUninitializedTable()
    {
        QVERIFY(!t->hasherIsSet());
    }

    void testFindInUnitializedTable()
    {
        try
        {
            t->find("oooops");
            QFAIL("HasherIsNotSetException wasn't thrown");
        }
        catch (HasherIsNotSetException const& e) {}
    }

    void cleanup()
    {
        delete t;
    }

private:
    HashTable<QString> *t;
};

QTEST_APPLESS_MAIN(HashTableTest)
