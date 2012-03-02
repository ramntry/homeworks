#include <iostream>
#include <cmath>
#include "oper.h"

using namespace std;

Oper::Oper() :
    operand(0),
    isOperand(true)
{
    cout << "Oper() has been called" << endl;
}

Oper::Oper(double num) :
    operand(num),
    isOperand(true)
{
    cout << "Oper(double num) has been called, num = " << num << endl;
}

Oper::Oper(char op, bool isOper) :
    operation(op),
    isOperand(isOper)
{                       // проверено: NAN с любым первым байтом
    if (isOper)         // тоже NAN, что позволяет хранить в Oper
    {                   // односимвольные операнды-переменные:
        operand = NAN;  // isOperand тогда true, но operand == NAN,
        operation = op; // то за полезной информацией путь в operation
    }
    cout << "Oper(char op, bool isOper) has been called, op = " << op << ", isOper = " << isOper << endl;
}

Oper::Oper(const Oper & src) :
    operand(src.operand),
    isOperand(src.isOperand)
{}

void Oper::out()
{
    if (isOperand)
        if (!isnan(operand))
            cout << operand;
        else
            cout << operation;
    else
        cout << operation;
}

