#pragma once
#include <sstream>
#include <QtCore/QString>
#include <QtTest/QtTest>
#include "HashTable.h"

class QStringHasher : public HashTable<QString>::Hasher
{
public:
    int operator ()(QString const& item)
    {
        return item[0].toLatin1();
    }
};

class HashTableTest : public QObject
{
    Q_OBJECT

private slots:
    void initTestCase()
    {
        hasher = new QStringHasher;
    }

    void init()
    {
        t = new HashTable<QString>;
        t->setHasher(hasher);
    }

    void testHasherIsSetInUninitializedTable()
    {
        HashTable<QString> tmp;
        QVERIFY(!tmp.hasherIsSet());
    }

    void testFindInUnitializedTable()
    {
        try
        {
            HashTable<QString> tmp;
            tmp.find("oooops");
            QFAIL("HasherIsNotSetException wasn't thrown");
        }
        catch (HasherIsNotSetException const& e) {}
    }

    void testAddOneItem()
    {
        t->add("Tru-tu-tu");
        QCOMPARE(t->find("Tru-tu-tu"), QString("Tru-tu-tu"));
    }

    void testFindNonExistItem()
    {
        t->add("la-la-la");
        t->add("boom-boom");

        QVERIFY(&t->find("la-la") == 0);
    }

    void testDel()
    {
        t->add("la-la-la");
        t->add("boom-boom");
        t->add("la-la-lam");

        t->del("la-la-la");

        QVERIFY(&t->find("la-la-la") == 0);
        QCOMPARE(t->find("la-la-lam"), QString("la-la-lam"));
    }

    void testPrintStat()
    {
        t->add("la-la-la");
        t->add("boom-boom");
        t->add("tra-ta-ta");
        t->add("la-la-lam");

        std::ostringstream out;
        t->printStat(out);

        std::string trueOut = "number of items: 4\n"
                              "load factor: 0.003\n"
                              "number of cells: 1000\n"
                              "number of busy cells: 3\n"
                              "number of conflicts: 1\n"
                              "max size of chain: 2\n";
        QCOMPARE(out.str(), trueOut);
    }

    void cleanup()
    {
        delete t;
    }

    void cleanupTestCase()
    {
        delete hasher;
    }

private:
    HashTable<QString> *t;
    HashTable<QString>::Hasher *hasher;
};

int hashTableTestExec(int argc, char **argv)
{
    HashTableTest htt;
    return QTest::qExec(&htt, argc, argv);
}
