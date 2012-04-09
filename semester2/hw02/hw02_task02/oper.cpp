#include <iostream>
#include <cmath>
#include "oper.h"

using namespace std;

Oper::Oper() :
    operand(0),
    isOperand(true)
{}

Oper::Oper(double num) :
    operand(num),
    isOperand(true)
{}

Oper::Oper(char op, bool isVar) :
    operation(op),
    isOperand(isVar)
{                       // проверено: NAN с любым первым байтом
    if (isVar)          // тоже NAN, что позволяет хранить в Oper
    {                   // односимвольные операнды-переменные:
        operand = NAN;  // isOperand тогда true, но operand == NAN,
        operation = op; // то за полезной информацией путь в operation
    }
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
