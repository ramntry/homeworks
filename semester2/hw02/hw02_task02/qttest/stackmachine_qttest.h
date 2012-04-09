#pragma once
#include <sstream>
#include <QtTest/QTest>
#include "../stackmachine.h"

class StackMachineTest : public QObject
{
    Q_OBJECT

private slots:
    void init()
    {
        sm = new StackMachine;
    }

    void cleanup()
    {
        delete sm;
    }

    void testSimpleAddition()
    {
        sm->put(1.0);
        sm->put(2.0);
        sm->put('+');

        QVERIFY(sm->isOK());
        QCOMPARE(sm->getValue(), 3.0);
    }

    void testComplexExpressionWithBasicOperations()
    {
        sm->putExpr("10 15 5 / - 3 2 * + 2 2 3 ^ ^ +");

        QVERIFY(sm->isOK());
        QCOMPARE(sm->getValue(), 269.0);
    }

    void testMemory()
    {
        sm->putExpr("a 10 15 5 / - 3 2 * + = b 2 2 3 ^ ^ = , c a b + = ,");

        QVERIFY(sm->isOK());
        QCOMPARE(sm->getValue(), 269.0);
        QCOMPARE(sm->getVar('c'), 269.0);
    }

    void testMemoryOut()
    {
        sm->putExpr("a 10 15 5 / - 3 2 * + = b 2 2 3 ^ ^ = , c a b + = ,");

        std::string trueOut("a = 13\nb = 256\nc = 269\n");
        std::ostringstream realOut;
        sm->memoryOut(realOut);

        QVERIFY(sm->isOK());
        QCOMPARE(realOut.str(), trueOut);
    }

    void testIsOK()
    {
        sm->putExpr("1 2 3 +");
        QVERIFY(sm->isOK() && sm->getValue() != sm->getValue());

        sm->putExpr("+ +");
        QVERIFY(!sm->isOK());
    }

private:
    StackMachine *sm;
};
