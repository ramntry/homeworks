#pragma once
#include <cstring>
#include <iostream>

/**
 * Класс-реализация абстракции вектора dim-мерного пространства.
 * Подразумевается инстанцирование шаблона с примитивными числовыми типами, такими
 * как int или double. Реализованы операции сложения, вычитания, умножения на скаляр,
 * расчета скалярного произведения и пр.
 */
template <typename N, size_t dim>
class Vector
{
public:
    Vector(N scalar = 0);

    template <typename I>
    Vector(I beginIt, I endIt);

    Vector<N, dim> add(Vector<N, dim> const& term) const;
    Vector<N, dim> sub(Vector<N, dim> const& term) const;
    Vector<N, dim> mul(N scalar) const;
    N scalarMul(Vector<N, dim> const& term) const;

    bool isNull() const;
    bool isScalar() const;
    bool isEqual(Vector<N, dim> const& term) const;

    N &operator [](size_t index)
    { return mArray[index]; }

protected:
    N mArray[dim];

template <typename T, size_t Dim>
friend std::ostream &operator <<(std::ostream &os, Vector<T, Dim> const& v);
};

template <typename N, size_t dim>
Vector<N, dim>::Vector(N scalar)
{
    memset(mArray, 0, sizeof(mArray));
    mArray[0] = scalar;
}

template <typename N, size_t dim>
template <typename I>
Vector<N, dim>::Vector(I it, I endIt)
{
    size_t done = 0;
    for (; it != endIt && done < dim; ++it, ++done)
        mArray[done] = *it;

    memset(mArray + done, 0, sizeof(mArray) - done * sizeof(N));
}

template <typename N, size_t dim>
Vector<N, dim> Vector<N, dim>::add(Vector<N, dim> const& term) const
{
    Vector<N, dim> tmp(*this);
    for (size_t i = 0; i < dim; ++i)
        tmp.mArray[i] += term.mArray[i];

    return tmp;
}

template <typename N, size_t dim>
Vector<N, dim> Vector<N, dim>::sub(Vector<N, dim> const& term) const
{
    Vector<N, dim> tmp(*this);
    for (size_t i = 0; i < dim; ++i)
        tmp.mArray[i] -= term.mArray[i];

    return tmp;
}

template <typename N, size_t dim>
Vector<N, dim> Vector<N, dim>::mul(N scalar) const
{
    Vector<N, dim> tmp(*this);
    for (size_t i = 0; i < dim; ++i)
        tmp.mArray[i] *= scalar;

    return tmp;
}

template <typename N, size_t dim>
N Vector<N, dim>::scalarMul(Vector<N, dim> const& term) const
{
    N acc = 0;
    for (size_t i = 0; i < dim; ++i)
        acc += mArray[i] * term.mArray[i];

    return acc;
}

template <typename N, size_t dim>
bool Vector<N, dim>::isEqual(Vector<N, dim> const& term) const
{
    for (size_t i = 0; i < dim; ++i)
        if (mArray[i] != term.mArray[i])
            return false;

    return true;
}

template <typename N, size_t dim>
bool Vector<N, dim>::isScalar() const
{
    for (size_t i = 1; i < dim; ++i)
        if (mArray[i] != 0)
            return false;

    return true;
}

template <typename N, size_t dim>
bool Vector<N, dim>::isNull() const
{
    return mArray[0] == 0 && isScalar();
}

template <typename T, size_t Dim>
std::ostream &operator <<(std::ostream &os, Vector<T, Dim> const& v)
{
    os << "Vector[" << Dim << "] ( ";
    for (size_t i = 0; i < Dim; ++i)
        os << v.mArray[i] << ' ';

    return os << ')';
}

int vectorTestExec(int argc, char **argv);
