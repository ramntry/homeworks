#pragma once
#include "list.h"

struct Monomial
{
    int degree;
    int koeff;

    Monomial(int d = 0, int k = 0):
        degree(d),
        koeff(k)
    {}
};

List<Monomial> createPolynomial(int *koeffs, int size);
int value(List<Monomial> &polynomial, int x);
