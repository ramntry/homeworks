#include "polynomial.h"
#include <cstdio>
#include <cmath>

using namespace std;

void fillPolynomial(Polynomial &p, int degree)
{
    for (int d = degree, ratio = 0; d >= 0 ; d--)
    {
        scanf("%d", &ratio);
        if (ratio)
            p.append(ratio, d);
    }
}

int valuePolynomial(Polynomial &p, int x)
{
    int lastValue = p.head->ratio;
    int lastDegree = p.head->degree;
    Monomial *current = p.head->next;

    while (current)
    {
        lastValue = lastValue * pow(x, lastDegree - current->degree) + current->ratio;
        lastDegree = current->degree;
        current = current->next;
    }
    return lastValue * pow(x, lastDegree);
}

bool isEqualPolynomial(Polynomial &pl, Polynomial &pr)
{
    Monomial *curLeft = pl.head;
    Monomial *curRight = pr.head;
    while (curLeft && curRight)
    {
        if ((curLeft->degree != curRight->degree)
         || (curLeft->ratio  != curRight->ratio))
            return false;
        curLeft = curLeft->next;
        curRight = curRight->next;
    }
    if (curLeft != NULL || curRight != NULL)
        return false;
    return true;
}

void appendTail(Polynomial &p, Monomial *lp, Monomial *rp)
{
    Monomial *from = lp ? lp : rp;
    while (from)
    {
        p.append(from->ratio, from->degree);
        from = from->next;
    }
}

void addPolynomial(Polynomial &summ, Polynomial &pl, Polynomial &pr)
{
    Monomial *lp = pl.head;
    Monomial *rp = pr.head;

    while (lp != NULL && rp != NULL)
    {
        if (lp->degree == rp->degree)
        {
            int ratio = lp->ratio + rp->ratio;
            if (ratio != 0)
                summ.append(ratio, lp->degree);
            lp = lp->next;
            rp = rp->next;
        }
        else if (lp->degree > rp->degree)
        {
            summ.append(lp->ratio, lp->degree);
            lp = lp->next;
        }
        else
        {
            summ.append(rp->ratio, rp->degree);
            rp = rp->next;
        }
    }
    appendTail(summ, lp, rp);
}

void printPolynomial(Polynomial &p, char *topRow, char *bottomRow)
{
    Monomial *current = p.head;
    while (current)
    {
        int writedTop = 0;
        int writedBottom = 0;
        if (abs(current->ratio) == 1)   // Единицы обрабатываем особо
        {
            if (!(current == p.head && current->ratio > 0))     // Перед первым x с коэффициентом +1.0 "+" ставить не будем
                writedBottom += sprintf(bottomRow, current->ratio > 0 ? "+" : "-");
            if (current->degree == 0)                           // Если это свободный член - то пишем его
                sprintf(bottomRow + writedBottom, "1");
        }
        else
        {
            if (current == p.head)
                writedBottom += sprintf(bottomRow, "%d", current->ratio);
            else
                writedBottom += sprintf(bottomRow, "%+d", current->ratio);
        }
        if (current->degree > 0)  // Если в записи должен фигурировать "x"
        {
            writedBottom += sprintf(bottomRow + writedBottom, "x");
            topRow += sprintf(topRow, "%*s", writedBottom, "");        // Верхнюю строку забиваем пробелами до нужной
            if (current->degree > 1)                                   // ... позиции
                writedTop += sprintf(topRow, "%d", current->degree);   // Пишем в верхнюю строку неединичную степень
            writedBottom += sprintf(bottomRow + writedBottom, "%*s", writedTop, "");
        }
        topRow += writedTop;
        bottomRow += writedBottom;
        current = current->next;
    }
}

void printPolynomial(Polynomial &p)
{
    char topRow[200];
    char bottomRow[200];
    topRow[0] = '\0';
    bottomRow[0] = '\0';

    printPolynomial(p, topRow, bottomRow);
    printf("%s\n%s\n", topRow, bottomRow);
}
