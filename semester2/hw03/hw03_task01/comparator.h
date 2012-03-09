#pragma once
#include <cstring>

const double standartDoubleEpsilon = 1.0e-14;

template <typename T>
class Comparator
{
public:
    virtual int operator ()(T const& a, T const& b) = 0;
};


template <typename T>
class StandartComparator : public Comparator<T>
{
public:
    int operator ()(T const& a, T const& b)
    {
        if (a < b)
            return -1;
        if (a > b)
            return 1;
        return 0;
    }
};

template <>
class StandartComparator<const char*> : public Comparator<const char*>
{
public:
    int operator ()(const char* const& a, const char* const& b)
    {
        return strcmp(a, b);
    }
};

template <>
class StandartComparator<double> : public Comparator<double>
{
public:
    StandartComparator<double>(double eps = standartDoubleEpsilon)
        : epsilon(eps)
    {}

    int operator ()(double const& a, double const& b)
    {
        double diff = a - b;
        if (diff < -epsilon)
            return -1;
        if (diff > epsilon)
            return 1;
        return 0;
    }

    double eps()
    {
        return epsilon;
    }

    void setEps(double newEps)
    {
        epsilon = newEps;
    }

private:
    double epsilon;
};
