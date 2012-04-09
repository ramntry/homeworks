#pragma once
#include <iostream>
#include <sstream>
#include "stack/stretchablestack.h"
#include "oper.h"

class StackMachine
{
    public:
        StackMachine();
        void put(Oper elem);                           // Основной приемный шлюз
        void put(double operand);                      // Перегрузки, необходимые для клиентского кода,
        void put(char operation, bool isVar = false);  // ... не использующего промежуточное oper-представление
        void putExpr(std::istream &in);                // Разбор простой строки в постфиксной нотации
        void putExpr(std::string const& str);
        double getValue();                             // Получение результата работы
        double getVar(char var);                       // Доступ к сохраненным переменным
        bool isOK();                                   // Флаг ошибок
        void memoryOut(std::ostream &os = std::cout);  // Печать содержимого памяти

    private:
        double memory[256];
        StretchableStack<Oper> stack;
        bool noError;
        void engine(char operation, Oper op1, Oper op2);
        double calc(char operation, double realOp1, double realOp2);
        double checkOperand(Oper op);
};

int stackMachineTestExec(int argc, char **argv);
