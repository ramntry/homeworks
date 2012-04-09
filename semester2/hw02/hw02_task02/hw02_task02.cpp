#include <iostream>
#include "stackmachine.h"

using namespace std;

int main(int argc, char **argv)
{
    int ret = stackMachineTestExec(argc, argv);
    if (ret)
        return ret;

    clog << "\nThis is a simple calculator.\n"
         << "Enter an expression in postfix polish notation and send EOF: " << endl;
    StackMachine stackMachine;
    stackMachine.putExpr(cin);

    if (stackMachine.isOK())
        cout << "= " << stackMachine.getValue() << endl;
    else
    {
        cerr << "Something is wrong in StackMachine, sorry" << endl;
        return 1;
    }

    return 0;
}
