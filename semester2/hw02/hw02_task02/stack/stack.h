#pragma once
#include <cstdlib>

template <class T>
class Stack
{
public:
    virtual ~Stack() {}

    virtual void push(T value) = 0;
    virtual T pop() = 0;

    virtual T look() const = 0;
    virtual bool isEmpty() const = 0;
    virtual int size() const = 0;
};

class StackUnderflowException {};
class StackOverflowException {};

int stackTestExec(int argc, char **argv);
