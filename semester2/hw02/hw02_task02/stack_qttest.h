#include <QtTest/QtTest>
#include "simplestack.h"
#include "stretchablestack.h"

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

/***************** </TESTS > *****************/

private:
    TestableStack *s;
};

int main(int argc, char **argv)
{
    TestableStack *stacks[] = {
                                new TestingStack<SimpleStack<ItemType> >("SimpleStack"),
                                new TestingStack<StretchableStack<ItemType> >("StretchableStack")
                              };

    int ret = 0;
    for (size_t i = 0; i < sizeof(stacks)/sizeof(void *); ++i)
    {
        qDebug() << "\n\n\t\t" << stacks[i]->name() << "\n";

        StackTest st(stacks[i]);
        ret |= QTest::qExec(&st, argc, argv);

        delete stacks[i];
    }
    return ret;
}
