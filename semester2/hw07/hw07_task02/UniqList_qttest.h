#include <QtTest/QTest>
#include "../../hw02/hw02_task01/arraylist.h"
#include "UniqueList.h"

class UniqueListTest : public QObject
{
    Q_OBJECT

private slots:
    void init()
    {
        ul = new UniqueList<ArrayList>;
    }

    void cleanup()
    {
        delete ul;
    }

    void testInsertExistingItem()
    {
        ul->insert(0, 3);
        ul->insert(0, 4);
        try
        {
            ul->insert(2, 3);
            QFAIL("InsertingExistingItemException wasn't thrown");
        }
        catch (InsertingExistingItemException const& e) {}
    }

    void testRemove()
    {
        ul->insert(0, 1);
        ul->insert(0, 2);
        ul->insert(2, 3);

        ul->remove(0);
        ul->insert(1, 4);

        ul->removeItem(1);
        try
        {
            ul->removeItem(1);
            QFAIL("RemovingNonExistingItemException wasn't thrown");
        }
        catch (RemovingNonExistingItemException const& e) {}

        QCOMPARE(ul->at(0), 4);
        QCOMPARE(ul->at(1), 3);
    }

private:
    UniqueList<ArrayList> *ul;
};

QTEST_APPLESS_MAIN(UniqueListTest)
