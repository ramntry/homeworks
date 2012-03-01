#include <iostream>
#include "stretchablestack.h"
#include "simplestack.h"

using namespace std;

int main()
{
    Stack *stacks[2] = { new StretchableStack(1), new SimpleStack(5) };

    for (int k = 0; k < 2; k++)
    {
        stacks[k]->push(10);
        stacks[k]->push(11);
        stacks[k]->push(12);
        stacks[k]->pop();
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
    return 0;
}
