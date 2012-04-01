#pragma once
#include "comparator.h"

/**
 *  Модуль sorter.h предоставляет несколько инструментов для манипулирования алгоритмами сортировки:
 *      1. Шаблонную функцию swap(T const& a, T const& b) для обмена аргументов местами в памяти.
 *      2. Абстрактный шаблонный класс Sorter<T, C> для реализации классов-стратегий сортировки.
 *      3. Макрос SORTER_METHOD(ClassName) для быстрого добавления пользовательских реализаций Sorter<T, C>
 *
 *  Модуль зависим от модуля comparator.h и реализует поддержку шаблонных классов-компараторов для пользовательских
 *  реализаций алгоритмов сортировок с его помощью. (см. документацию к модулю comparator.h)
 *
 *  Пусть в системе уже существует класс-стратегия BubbleSorter<T, C>. С его помощью возможна сортировка массива
 *  типа T* yourArray известного размера size, с использованием компаратора comp следующим образом:
 *      BubbleSorter<double> sorter(); // Шаблонный параметр C (Comparator) имеет значение по умолчанию
 *      sorter.sort(yourArray, size);                                              StandartComparator<T>
 *  где comp - callable-объект класса С, инстанцируемый с использованием конструктора по умолчанию ( new С() )
 *
 *  Изменить поведение компаратора возможно следующими способами:
 *      1. Инстанцировать sorter-объект с соответствующим параметром-типом:
 *             BubbleSort<double, CustomComparatorType>;
 *      2. Получить доступ к текущему компаратору sorter-объекта и воспользоваться его внутренними средствами
 *         управления поведением:
 *             sorter.comparator().setEps(1.0);
 *      3. Временно, runtime, осуществить подмену текущего компаратора при вызове метода класса sort(...):
 *             CustomSomparatorType *myComp = new CustomComparatorType(...);
 *             sorter.sort(yourArray, size, myComp);
 *         ВАЖНО: Ответственность за уничтожение временного компаратора лежит на клиентском коде.
 *
 *  Добавить в иерархию пользовательскую стратегию возможно двумя основными способами:
 *      1. Воспользоваться макросом SORTER_METHOD(ClassName). В этом случае заголовочный файл CustomSorter.h
 *         может иметь примерно следующее содержимое:
 *             #pragma once
 *             #include "sorter.h"
 *
 *             SORTER_METHOD(CustomSorter)
 *             {
 *                 // Реализация чистого виртуального метода класса Sorter<T, C>::
 *                 // _sort(T* data, size_t size, Comparator<T> &comp)
 *             }
 *         Внутри реализации метода sort(...) генерируемого класса CustomSorter доступны:
 *             1) Параметры шаблона T и С
 *             2) Массив data типа T*
 *             3) Размер массива size
 *             4) Процедура swap(...)
 *             5) Callable-объект comp(...)
 *         Последний принимает два аргумента a и b (comp(T const& a, T const& b)), возвращает целочисленное (int)
 *         значение: отрицательно, если a < b, положительное в противном случае, 0 при строгом равенстве a и b.
 *
 *         Использование добавляемого таким образом класса станет доступным примерно следующим образом:
 *             #include "CustomSorter.h"
 *
 *             CustomSorter<const char*, [CustomComparatorType]> sorter();
 *             sorter.sort(yourArrayOfCStrings, size, [temporaryComparatorObject])
 *
 *      2. Самостоятельно осуществить наследование от класса Sorter<T, C>. См. исходный код модуля.
 */


template <typename E>
inline void swap(E &a, E &b)
{
    E c(a);
    a = b;
    b = c;
}

template <typename T, typename C = StandartComparator<T> >
class Sorter
{
public:
    Sorter()
        : mComparator(new C())
    {}
    virtual ~Sorter() { delete mComparator; }
    C &comparator() { return *mComparator; }

    void sort(T* data, size_t size, Comparator<T> *comp = 0)
    { _sort(data, size, comp ? *comp : *mComparator); }

protected:
    virtual void _sort(T* data, size_t size, Comparator<T> &comp) = 0;
    C *mComparator;
};

#define SORTER_METHOD(ClassName) \
template <typename T, typename C = StandartComparator<T> >              \
class ClassName : public Sorter<T, C>                                   \
{                                                                       \
public:                                                                 \
    void _sort(T* data, size_t size, Comparator<T> &comp);              \
};                                                                      \
                                                                        \
template <typename T, typename C>                                       \
void ClassName<T, C>::_sort(T* data, size_t size, Comparator<T> &comp)
