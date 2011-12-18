#pragma once
#include <cstdio>

struct Monomial
{
    int ratio;
    int degree;

    Monomial *next;

    Monomial(int r, int d) :
        ratio(r),
        degree(d),
        next(NULL)
    {}
};

struct Polynomial
{
    Monomial *head;
    Monomial **joinMe;

    Polynomial(Monomial *first = NULL) :
        head(first),
        joinMe(&head)
    {}

    ~Polynomial()
    {
        Monomial *current = head;
        while (current)
        {
            Monomial *tmp = current;
            current = current->next;
            delete tmp;
        }
    }

    void clear() {
        this->~Polynomial();
        head = NULL;
        joinMe = &head;
    }

    void append(int ratio, int degree)
        { joinMe = &( (*joinMe = new Monomial(ratio, degree) )->next); }
};

void fillPolynomial(Polynomial &p, int degree);
void printPolynomial(Polynomial &p);
int valuePolynomial(Polynomial &p, int x);
bool isEqualPolynomial(Polynomial &pl, Polynomial &pr);
void addPolynomial(Polynomial &dst, Polynomial &pl, Polynomial &pr);
