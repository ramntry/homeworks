#include "stack.h"
#include <cstdlib>

namespace Stack
{
    struct stack  // Стэк собирается на односвязном списке
    {
        char v;
        stack * last;
    };
    
    stack * new_stack()
    {
        stack * s = new stack;
        s->last = NULL;
        return s;
    }

    void push(stack * &s, const char value) // Указатель вершины стэка
    {                                       // содержит адрес следующего
        s->v = value;                       //элемента. Последнее актуальное
        stack * next = new stack;           //значение см. в предыдущем узле
        next->last = s;
        s = next;
    }

    char pop(stack * &s)
    {
        stack * toDel = s;
        s = s->last;
        delete toDel;
        return s->v;
    }

    char look(stack * s)
    {
        return s->last->v;
    }

    bool isEmpty(const stack * s)
    {
        return s->last == NULL;
    }

    void erase(stack * &s)
    {
        while (s->last != NULL)
        {
            stack * toDel = s;
            s = s->last;
            delete toDel;
        }
        delete s;
    }

};

