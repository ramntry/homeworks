#ifndef STACK_H
#define STACK_H

namespace Stack
{
    struct stack;
    stack * new_stack();
    void push(stack * &s, const char value);
    char pop(stack * &s);
    char look(stack * s);
    bool isEmpty(const stack * s);
    void erase(stack * &s);
};

#endif // #ifndef STACK_H

