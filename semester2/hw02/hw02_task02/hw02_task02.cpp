/*
#include <iostream>
#include "stretchablestack.h"
#include "simplestack.h"
#include "stackmachine.h"

using namespace std;

int StacksTest()
{
    Stack<int> *stacks[2] = { new StretchableStack<int>(1), new SimpleStack<int>(5) };

    for (int k = 0; k < 2; k++)
    {
        stacks[k]->push(10);
        stacks[k]->push(11);
        stacks[k]->push(12);
        cout << "before pop() top is a " << stacks[k]->look() << endl;;
        stacks[k]->pop();
        cout << "after pop() top is a " << stacks[k]->look() << endl;;
        stacks[k]->push(22);
        stacks[k]->push(13);
        stacks[k]->pop();
        stacks[k]->push(23);
        stacks[k]->push(14);

        while (!stacks[k]->isEmpty())
            cout << stacks[k]->pop() << " ";
        cout << endl;

        delete stacks[k];
    }

    Stack<int> *bigStack = new StretchableStack<int>(1);

    for (int i = 0; i < 1024 * 1024; i++)
        bigStack->push(i);
    for (int i = 0; i < 900 * 1024; i++)
        bigStack->pop();
    for (int i = 0; i < 32 * 1024 * 1024; i++)
        bigStack->push(i);

    delete bigStack;

    return 0;
}

int main()
{
    clog << "Testing of classes SimpleStack and StretchableStack:\n" << endl;
    StacksTest();

    clog << "\nThis is a simple calculator.\n"
         << "Enter an expression in postfix polish notation and send EOF: " << endl;
    StackMachine stackMachine;

    char ch = ' ';
    double operand = 0.0;

    while (true)
    {
        do
            cin >> ch;
        while (isspace(ch));
        if (!cin)
            break;

        if (isdigit(ch))
        {
            cin.putback(ch);
            cin >> operand;
            stackMachine.put(operand);
        } else {
            stackMachine.put(ch);
        }
    }

    if (stackMachine.isOK())
        cout << "= " << stackMachine.getValue() << endl;
    else
        cerr << "Something is wrong in StackMachine, sorry" << endl;

    return 0;
}
*/

#include "stackmachine.h"

int main(int argc, char **argv)
{
    return stackMachineTestExec(argc, argv);
}

