#pragma once
#include <cstdlib>

typedef int StackElement;

class Stack
{
public:
    virtual ~Stack() {}

    virtual void push(StackElement value) = 0;
    virtual StackElement pop() = 0;

    virtual StackElement look() const = 0;
    virtual bool isEmpty() const = 0;
    virtual int size() const = 0;
};

class StackUnderflowException {};
class StackOverflowException {};
