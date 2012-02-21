// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 01, task 05, v2

// Написать программу проверки баланса скобок в исходной
// строке (т.е. число открывающих скобок равно числу
// закрывающих и выполняется правило вложенности скобок)

// [time start] 20:00 17.09.11
// [time estimate] 00:50 00


#include <iostream>
#include "stack.h"

using namespace std;

const char brackets[] = {'(', ')', '<', '>', '[', ']', '{', '}'};
const int numofBrackets = 8;

int index(const char bracket)
{
    int i = 0;
    while (brackets[i] != bracket && i < numofBrackets)
        i++;
    return i;
}

inline bool isOpening(const char bracket)
{
    return index(bracket) % 2 == 0;
}

bool hasOpening(Stack::stack * &s, const char bracket)
{   // Проверяет, найдется ли в стеке открывающая скобка
    if (Stack::isEmpty(s)  // того же типа, что и переданная закрывающая
    ||  Stack::look(s) != brackets[index(bracket) - 1]) 
        return false;
    Stack::pop(s); 
    return true;
}


int main()
{
    clog << "This program expects from standard "
         << "input sequence of brackets of various\n"
         << "types, terminated by signal of the end of the file "
         << "and determines if the\n"
         << "bracket balance has been broken"
         << endl;

   Stack::stack * s = Stack::new_stack();

   int counter = 0;
   char bracket = ')';
   for(;;)                         // Пока не пуст входной поток
   {
       cin >> bracket;             // получить следующую скобку
       if (cin.eof())
           break;
       counter++;
       if (isOpening(bracket))     // сохранить ее в стек, если открывающая
           Stack::push(s, bracket);
       else                             // иначе попытаться удалить
           if (!hasOpening(s, bracket)) // с вершины стека
               break;             // соответсвующую ей открывающую, которая
   }                              // должна там быть
   
                                  // Если был обнаружен дисбаланс скобок
   if (!cin.eof() || !Stack::isEmpty(s))  
   {                                     // или не хватает закрывающих
       Stack::erase(s);  // Удаляем ДО довыборки входного потока - во время
                         // неё нет необходимости занимать память
       cout << "Bracket balance was broken at position "
            << counter << endl;
       while (!cin.eof())                // вывести сообщение и довыбрать
           cin.get();                    // входной поток (предполагая, что

       return 1;
   }                                     // пользователь расчитывает на это)

   Stack::erase(s);  // [v3] Введено корректное удаление стека
   cout << "Bracket balance was NOT broken" << endl;
   
   return 0;
}


// [time done] 23:46 17.09.11
// [time real] 03:46 00

