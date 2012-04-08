#pragma once
#include <QtTest/QtTest>
#include <sstream>
#include "../../../semester1/hw06/task02/polish_tree.h"

class PolishTreeTest : public QObject
{
    Q_OBJECT

private slots:
    void init()
    {
        s  = new std:: stringstream;
        os = new std::ostringstream;
    }

    void cleanup()
    {
        delete  s;
        delete os;
    }

    void testSimpleAddition()
    {
        check("(+1 1)", " (+ 1 1)", 2);
    }

    void testAdditionWithRubbish()
    {
        check("     (\n\t+\t\t1          \n   1\t)\n\n ", " (+ 1 1)", 2);
    }

    void testOneCompositeArgument()
    {
        check(" (- (* 2 3) 5)", 1);
    }

    void testTwoCompositeArguments()
    {
        check(" (/ (- 48 16) (* 2 4))", 4);
    }

    void testBigExperssion()
    {
        check(" (* (/ 60 4) (= (+ (/ (- 48 16) (* 2 4)) (- 7 4)) 7))", 15);
    }

private:
    std::stringstream *s;
    std::ostringstream *os;

    void check(std::string const& expr, std::string const& expStr, int expRes)
    {
        *s << expr;
        OperationNode on(*s);
        *os << on;

        QCOMPARE(on.calculate(), expRes);
        QCOMPARE(os->str(), expStr);
    }

    void check(std::string const& expr, int expRes)
    {
        check(expr, expr, expRes);
    }
};

QTEST_APPLESS_MAIN(PolishTreeTest)

