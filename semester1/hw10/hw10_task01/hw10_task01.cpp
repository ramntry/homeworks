#include "polynomial.h"
#include <cstdio>

int main()
{

    printf("Enter a degree of your polynome: ");
    int degree = 0;
    scanf("%d", &degree);

    int *koeffs = new int[degree + 1];
    printf("... and coefficients: ");
    for (int i = 0; i <= degree; i++)
        scanf("%d", koeffs + i);

    List<Monomial> polynomial = createPolynomial(koeffs, degree + 1);
    int arg = 0;
    printf("Enter x: ");
    scanf("%d", &arg);
    printf("%d\n", value(polynomial, arg));
}
