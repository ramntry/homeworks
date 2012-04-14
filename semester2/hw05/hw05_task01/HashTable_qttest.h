#pragma once
#include <sstream>
#include <QtCore/QString>
#include <QtTest/QtTest>
#include "HashTable.h"
#include "QStringHashers.h"

class HashTableTest : public QObject
{
    Q_OBJECT

private slots:
    void initTestCase()
    {
        hasher = new QStringHasherFirstChar;
    }

    void init()
    {
        t = new HashTable<QString>(8);
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

    void testAddSameItem()
    {
        t->add("zzz");
        t->add("zzz");

        QCOMPARE(t->size(), (size_t) 1);
    }

    void testDel()
    {
        t->add("la-la-la");
        t->add("boom-boom");
        t->add("la-la-lam");

        QVERIFY(t->del("la-la-la"));
        QCOMPARE(t->size(), (size_t) 2);

        QVERIFY(&t->find("la-la-la") == 0);
        QCOMPARE(t->find("la-la-lam"), QString("la-la-lam"));

        QVERIFY(!t->del("la-la-la"));
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
                              "load factor: 0.5\n"
                              "number of cells: 8\n"
                              "number of busy cells: 2\n"
                              "number of conflicts: 2\n"
                              "max size of chain: 3\n";
        QCOMPARE(out.str(), trueOut);
    }

    void testSetNewHasher()
    {
        t->add("la-la-la");
        t->add("boom-boom");
        t->add("tra-ta-ta");
        t->add("la-la-lam");

        HashTable<QString>::Hasher *h = new QStringHasherTwoChars;
        t->setHasher(h);

        t->del("boom-boom");

        QVERIFY(&t->find("la-la-la"));
        QVERIFY(&t->find("la-la-lam"));
        QVERIFY(&t->find("tra-ta-ta"));
        QVERIFY(!&t->find("boom-boom"));

        delete h;
    }

    void testBig()
    {
        QString pattern = "abracadabra";
        for (int i = 0; i < 20; ++i)
        {
            QString tmp = pattern;
            tmp[0] = tmp[0].toAscii() + i;
            for (int j = 0; j < 20; ++j)
            {
                t->add(tmp);
                tmp[1] = tmp[1].toAscii() + 1;
            }
        }

        HashTable<QString>::Hasher *h = new QStringHasherTwoChars;
        t->setHasher(h);

        for (int i = 0; i < 20; ++i)
        {
            QString tmp = pattern;
            tmp[0] = tmp[0].toAscii() + i;
            for (int j = 0; j < 20; ++j)
            {
                QVERIFY(&t->find(tmp));
                t->del(tmp);
                QVERIFY(!&t->find(tmp));

                tmp[1] = tmp[1].toAscii() + 1;
            }
        }
        QCOMPARE(t->size(), (size_t) 0);

        delete h;
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
    int ret = QTest::qExec(&htt, argc, argv);

    std::cout << "\n\n" << std::endl;
    return ret;
}
