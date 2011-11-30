#include "polynomial.h"

List<Monomial> createPolynomial(int *koeffs, int size)
{
    List<Monomial> polynomial;
    int powRank = size - 1;
    for (int i = 0; i < size; i++)
    {
        if (koeffs[i] != 0)
            polynomial.push(Monomial(powRank, koeffs[i]));
        powRank--;
    }
    return polynomial;
}

int value(List<Monomial> &polynomial, int x)
{
    int currentPow = 0;
    int accumulator = 0;
    int currentArg = 1;
    int size = polynomial.size();
    for (int i = 0; i < size; i++)
    {
        Monomial current = polynomial[i];
        while (current.degree - currentPow > 0)
        {
            currentArg *= x;
            currentPow++;
        }
        accumulator += currentArg * current.koeff;
    }
    return accumulator;
}

bool equals(List<Monomial> &left, List<Monomial> &right);
