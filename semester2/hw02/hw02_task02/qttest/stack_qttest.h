#include <QtTest/QtTest>
#include <cstring>
#include "stack/simplestack.h"
#include "stack/stretchablestack.h"

typedef int ItemType;

class TestableStack : virtual public Stack<ItemType>
{
public:
    TestableStack(const char *_name)
        : m_name(_name)
    {}

    virtual void reset() = 0;

    const char *name()
    { return m_name; }

protected:
    const char *m_name;
};

template <typename T>
class TestingStack : public TestableStack, protected T
{
public:
    TestingStack(const char *_name)
        : TestableStack(_name)
    {}

    void reset()
    {
        this->~TestingStack();
        new (this) TestingStack(m_name);
    }
};

class StackTest : public QObject
{
    Q_OBJECT

public:
    StackTest(TestableStack *ts)
        : s(ts)
    {}

private slots:
    void cleanup()
    {
        s->reset();
    }

/***************** < TESTS > *****************/

    void testIsEmptyInEmptyStack()
    {
        QVERIFY(s->isEmpty());
    }

    void testSizeInEmptyStack()
    {
        QCOMPARE(s->size(), 0);
    }

    void testSimplePush()
    {
        s->push(100);
        QCOMPARE(s->size(), 1);
    }

    void testSimplePop()
    {
        s->push(200);
        QCOMPARE(s->pop(), 200);
        QVERIFY(s->isEmpty());
    }

    void testSimpleLook()
    {
        s->push(300);
        QCOMPARE(s->look(), 300);
        QCOMPARE(s->size(), 1);
    }

    void testComposite()
    {
        for (int i = 0; i < 10; ++i)
            s->push(i);
        QCOMPARE(s->look(), 9);

        int acc = 0;
        for (int i = 0; i < 5; ++i)
            acc += s->pop();
        QCOMPARE(acc, 35);

        for (int i = 0; i < 10; ++i)
            s->push(i * 2);
        QCOMPARE(s->pop(), 18);
        QCOMPARE(s->size(), 14);
    }

    void testBig()
    {
        try
        {
            for (int i = 0; i < 100000; ++i)
                s->push(i);

            for (int i = 0; i < 50000; ++i)
                s->pop();

            for (int i = 0; i < 100000; ++i)
                s->push(i);

            QCOMPARE(s->size(), 150000);
        }
        catch (StackOverflowException const& e)
        {
            if (strcmp(s->name(), "SimpleStack"))
                QFAIL("StackOverflowException in non-SimpleStack");
        }
    }

    void testPopFromEmptyStack()
    {
        try
        {
            s->pop();
            QFAIL("StackUnderflowException wasn't thrown");
        }
        catch (StackUnderflowException const& e) {}
    }

    void testLookFromEmptyStack()
    {
        try
        {
            s->look();
            QFAIL("StackUnderflowException wasn't thrown");
        }
        catch (StackUnderflowException const& e) {}
    }

/***************** </TESTS > *****************/

private:
    TestableStack *s;
};

