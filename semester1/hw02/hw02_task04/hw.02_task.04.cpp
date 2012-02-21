// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 02, task 04

// Напечатать в порядке возрастания все
// простые несократимые дроби, заключенные между
// 0 и 1, знаменатели которых не превышают N

// [time start] 23:35 27.09.11
// [time estimate] 00:40 00


#include <iostream>

using namespace std;

struct list
{
    int numer;
    int denom;
    list * next;
    list() : next(NULL), numer(0), denom(1) {}
};

list * makeFractions(int maxDenom)
{
    list * fractions = new list;
    fractions->next = new list;
    fractions->next->numer = 1;

    while (fractions->next->denom != maxDenom)
    {
        list * cur = fractions;
        while (cur->next != NULL)
        {
            int nextDenom = cur->denom + cur->next->denom;
            if (nextDenom <= maxDenom)
            {
                list * intermed = new list;
                intermed->numer = cur->numer + cur->next->numer;
                intermed->denom = nextDenom;
                intermed->next = cur->next;
                cur->next = intermed;
                cur = intermed->next;
            } else
                cur = cur->next;
        }
    }
    return fractions;
}

void eraseList(list * fractions)
{
    list * toDel;
    while (fractions != NULL)
    {
        toDel = fractions;
        fractions = fractions->next;
        delete toDel;
    }
}

int main()
{
    clog << "This program displays all simple fractions from 0 to 1 with\n"
         << "denominator not exceeding n\nEnter\tn: ";
    int n = 0;
    cin >> n;

    list * fractions = makeFractions(n);
    list * cur = fractions->next;
    clog << "Fractions: ";
    while (cur->next != NULL)
    {
        cout << cur->numer << '/'<< cur->denom << ' ';
        if ((double)cur->numer / (double)cur->denom  >
            (double)cur->next->numer / (double)cur->next->denom)
            cerr << "Error! Incorrect algorithm" << endl;
        cur = cur->next;
    }
    cout << endl;
    eraseList(fractions);
    return 0;
}


// [time done] 02:14 28.09.11
// [time real] 02:39 00

