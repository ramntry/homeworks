#pragma once
#include "comparator.h"

template <typename E>
void swap(E &a, E &b)
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
    ~Sorter() { delete mComparator; }
    virtual void sort(T* data, size_t size) = 0;
    C &comparator() { return *mComparator; }

protected:
    C *mComparator;
};

//Test heir:
template <typename T, typename C = StandartComparator<T> >
class BubbleSorter : public Sorter<T, C>
{
public:
    BubbleSorter()
        : Sorter<T, C>()
    {}

    void sort(T* data, size_t size)
    {
        for (int i = size - 1; i > 0; --i)
            for (int j = 0; j < i; j++)
        // ERROR:     В строке [1] я не могу обратиться к защищенному полю родительского класса - его, несмотря на
        //        наследование, просто нет в текущей области видимости.
        //            В строке [2], очевидно, сохраняется та же проблема, но сообщение о ней перекрывается другой
        //        ошибкой - не удается вывод шаблона. Совершенно непонятно, почему.
        //            В строках [3] и [4] - рабочие и равнозначные варианты
        //        if ((*mComparator)(data[j], data[j + 1]) > 0)               // [1]
        //        if (comparator()(data[j], data[j + 1]) > 0)                 // [2]
                if (Sorter<T, C>::comparator()(data[j], data[j + 1]) > 0)   // [3]
        //        if ((*Sorter<T, C>::mComparator)(data[j], data[j + 1]) > 0) // [4]
                    swap(data[j], data[j + 1]);
    }
};
