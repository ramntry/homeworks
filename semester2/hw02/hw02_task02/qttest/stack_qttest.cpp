#include "stack_qttest.h"
#include "../stack/simplestack.h"
#include "../stack/stretchablestack.h"

int stackTestExec(int argc, char **argv)
{
    TestableStack *stacks[] =
                              { new TestingStack<SimpleStack<ItemType> >("SimpleStack")
                              , new TestingStack<StretchableStack<ItemType> >("StretchableStack")
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
