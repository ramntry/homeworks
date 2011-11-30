#pragma once

#include <iostream>

template <class Type>
struct ListNode;

template <class Type>
class List
    // Односвязный шаблонный циклический список с охранником и
    // оптимизированной схемой произвольного доступа
{
public:
    List();
    ~List();

    // Стеково-очередные методы
    List & push(const Type & item);    // положить в стек
    List & append(const Type & item);  // добавить в конец очереди
    Type pop();               // снять со стека / из очереди
    Type look();              // только посмотреть очередной элемент
    bool pushOut();           // только удалить из стека / очереди

    // Методы произвольного доступа (не определены при пустом списке)
    Type & operator[](int index);       // доступ по индексу
    List & insert(int index, const Type & item); // вставка в
                                                 // указанную позицию
    List & erase(int index);            // удаление в указанной позиции
    int find(const Type & item);   // возврат индекса первого найденного
                                   // элемента или -1 в случае неудачи

    // Общие методы
    bool isEmpty();
    int  size();
    int getCursorOffset();
    List & out();

private:
    static const int badOffset = -1;

    ListNode<Type> * guard;  // вспомогательный элемент - и голова, и хвост
    ListNode<Type> * cursor; // указатель текущей позиции в списке (->next)
    int offset;          // положение курсора
    int _size;
    ListNode<Type> * getCursor(int index);  // быстро при возрастающем рядe 
    void insertAfter(ListNode<Type> * prev, const Type & item);
    void insertAfter(ListNode<Type> * prev);
    void eraseListNode(ListNode<Type> * toErase);
};

template <class Type>
struct ListNode
{
    Type value;
    ListNode * next;
    
    ListNode(ListNode * linkWith, const Type & item) :
        value(item),
        next(linkWith)
    {}

    ListNode(ListNode * linkWith) :
        next(linkWith)
    {}

    ListNode() :
        next(this)          // пустой список - зацикленный охранник
    {}
};

/*************************************/

template <class Type>
List<Type>::List() :
    guard(new ListNode<Type>),
    cursor(guard),
    offset(badOffset),
    _size(0)
{}

template <class Type>
List<Type> & List<Type>::push(const Type & item)
{
    insertAfter(guard, item);
    offset = badOffset;        // из-за наличия охранника любая подобная
                               // операция потенциально рушит курсор
    return *this;
}

template <class Type>
List<Type> & List<Type>::append(const Type & item)
{
    guard->value = item;     // g->['\0']        => g->['a']
    insertAfter(guard);      // g->['a']         => g->['a']->['\0']
    guard = guard->next;     // g->['a']->['\0'] => ['a']->['\0']<-g
    offset = badOffset;
    return *this;
}

template <class Type> inline
void List<Type>::insertAfter(ListNode<Type> * prev, const Type & item)
{
    prev->next = new ListNode<Type>(prev->next, item);
    _size++;
}

template <class Type> inline
void List<Type>::insertAfter(ListNode<Type> * prev)
{
    prev->next = new ListNode<Type>(prev->next);
    _size++;
}

template <class Type> inline
Type List<Type>::look()
{
    return guard->next->value;
}

template <class Type>
bool List<Type>::pushOut()
{
    if (!isEmpty())
        eraseListNode(guard);
    offset = badOffset;
    return !isEmpty();
}

template <class Type>
Type List<Type>::pop()
{
    Type item(look());
    pushOut();
    return item;
}

template <class Type>
void List<Type>::eraseListNode(ListNode<Type> * toErase)
{
    ListNode<Type> * rubbish = toErase->next;
    toErase->next = rubbish->next;
    delete rubbish;
    _size--;
}

template <class Type>
ListNode<Type> * List<Type>::getCursor(int index)
{
    index %= _size;                  // Курсор и вся работа с ним призвана
    int distance = index - offset;   // ускорить операции произвольного
    if (offset == badOffset || distance < 0)  // доступа вo
    {                                // всех случаях, когда следующий индекс
        cursor = guard;              // расположен дальше по списку
        for (int i = 0; i < index; i++)
            cursor = cursor->next;
    } else
    {
        for (int i = 0; i < distance; i++)
            cursor = cursor->next;
    }
    offset = index;
    return cursor;
}

template <class Type> inline
Type & List<Type>::operator[](int index)
{
    return getCursor(index)->next->value;
}

template <class Type>
List<Type> & List<Type>::insert(int index, const Type & item)
{
    insertAfter(getCursor(index), item);
    return *this;
}

template <class Type>
List<Type> & List<Type>::erase(int index)
{
    eraseListNode(getCursor(index));
//    std::cout << "Offset: " << offset << std::endl;
    return *this;
}

template <class Type>
int List<Type>::find(const Type & item)
{
    for (int i = 0; i < _size; i++)
        if (getCursor(i)->next->value == item)
            return i;
    return -1;
}

template <class Type> inline
bool List<Type>::isEmpty()
{
    return guard->next == guard;
}

template <class Type> inline
int List<Type>::size()
{
    return _size;
}
       
template <class Type> inline
int List<Type>::getCursorOffset()
{
    return offset;
}
       
template <class Type>
List<Type> & List<Type>::out()
{
    for (int i = 0; i < _size; i++)
        std::cout << getCursor(i)->next->value << ' ';
    std::cout << std::endl;
    return *this;
}

template <class Type> inline
List<Type>::~List()
{
    while(pushOut());
    delete guard;
}
