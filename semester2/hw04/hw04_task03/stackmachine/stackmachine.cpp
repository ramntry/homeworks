#include <iostream>
#include <cmath>
#include "oper.h"
#include "stackmachine.h"

using namespace std;

double StackMachine::calc(char operation, double realOp2, double realOp1)
{
    double res = NAN;
    switch (operation)
    {
        case '+':
            res = realOp2 + realOp1;
            break;                 
        case '-':                  
            res = realOp2 - realOp1;
            break;                 
        case '*':                  
            res = realOp2 * realOp1;
            break;                 
        case '/':                  
            res = realOp2 / realOp1;
            break;
        case '^':
            res = pow(realOp2, realOp1);
            break;
        case ',':
            res = realOp1;
            break;
        default:
            noError = false;
    }
    return res;
}

void StackMachine::put(double operand)
{
    if (!noError)
        return;
    stack.push(Oper(operand));
}

void StackMachine::put(char operation)
{
    if (!noError)
        return;
    if (stack.size() < 2)  // Пока работают только бинарные
        noError = false;   // операции
    else
    {
        Oper leftOp = stack.pop();
        engine(operation, leftOp, stack.pop());
    }
}

void StackMachine::put(Oper elem)
{
    if (elem.isOperand)
    {
        if (noError)
            stack.push(elem);
    }
    else
        put(elem.operation);
}

double StackMachine::checkOperand(Oper op)
{
    double realOp = NAN;
    if (isnan(op.operand))
    {
        realOp = memory[(int)op.operation];
        if (isnan(realOp))
            noError = false;
    } else
        realOp = op.operand;
    return realOp;
}

void StackMachine::engine(char operation, Oper op2, Oper op1)
{
    double realOp1 = checkOperand(op1);
    if (operation == '=')
    {
        if (!isnan(op2.operand))
            noError = false;
        else
            memory[(int)op2.operation] = realOp1;
            stack.push(Oper(realOp1));
        return;
    }
    double realOp2 = checkOperand(op2);
    if (!noError)
        return;

    double res = calc(operation, realOp2, realOp1);
    stack.push(Oper(res));
}

StackMachine::StackMachine() :
    stack(),
    noError(true)
{
    for (int i = 0; i < 256; i++)
        memory[i] = NAN;
}

bool StackMachine::isOK()
{
    return noError;
}

double StackMachine::getValue()
{
    if (stack.size() == 1 && noError)
        return stack.look().operand;
    return NAN;
}

double StackMachine::getVar(char var)
{
    return memory[(int)var];
}

void StackMachine::memoryOut()
{
    for (int i = 0; i < 256; i++)
        if (!isnan(memory[i]))
            cout << static_cast<char>(i) << " = "
                 << memory[i] << endl;
}

