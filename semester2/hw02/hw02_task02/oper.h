#pragma once

struct Oper
{
    union
    {
        double operand;
        char operation;
    };
    bool isOperand;

    Oper();
    Oper(double num);
    Oper(char op, bool isVar=false);
    Oper(const Oper & src);
    void out();
};

