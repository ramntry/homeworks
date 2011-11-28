#include <cstdio>
#include <cmath>

void printPolynome(double polynome[], int size, char topRow[], char bottomRow[], int precision = 2)
{
    double eps = pow(10.0, -precision);
    int powRank = size - 1;
    for (int i = 0; i < size; i++)
    {
        int writedTop = 0;
        int writedBottom = 0;
        if (fabs(polynome[i]) >= eps)
        {
            if (fabs(fabs(polynome[i]) - 1.0) < eps)
            {
                if (!(i == 0 && polynome[0] > 0))
                    writedBottom += sprintf(bottomRow, polynome[i] > 0.0 ? "+" : "-");
                if (powRank == 0)
                sprintf(bottomRow + writedBottom, "%.*lf", precision, 1.0);

            }
            else
                writedBottom += sprintf(bottomRow, "%+.*lf", precision, polynome[i]);
            if (powRank > 0)
            {
                writedBottom += sprintf(bottomRow + writedBottom, "x");
                topRow += sprintf(topRow, "%*s", writedBottom, "");
                if (powRank > 1)
                    writedTop += sprintf(topRow, "%d", powRank);
                writedBottom += sprintf(bottomRow + writedBottom, "%*s", writedTop, "");
            }
        }
        topRow += writedTop;
        bottomRow += writedBottom;
        powRank--;
    }
}

void printPolynomeSmartPrecision(double polynome[], int size, char topRow[], char bottomRow[])
{
    int precision = 0;
    for (int i = 0; i < size; i++)
    {
        double frPart = fabs(1.0 / (polynome[i] - (int) polynome[i]));
        if (isnan(frPart))
            continue;
        int newPrecision = ceil(log10(frPart));
        if (newPrecision > precision)
            precision = newPrecision;
    }
    printPolynome(polynome, size, topRow, bottomRow, precision);
}

int main(void)
{
    printf("This program prints a polynome like this:\n");

    char topRow[1024];
    topRow[0] = '\0';
    char bottomRow[1024];
    bottomRow[0] = '\0';

    double polynomeTest[] = {1.001, 2.5, -3, 0, 0, 0.001, -1, 0, 0, 0, -1};
    printPolynome(polynomeTest, sizeof(polynomeTest) / sizeof(double), topRow, bottomRow);
    printf("%s\n%s\nEnter a degree of your polynome: ", topRow, bottomRow);

    int degree = 0;
    topRow[0] = '\0';
    bottomRow[0] = '\0';
    scanf("%d", &degree);

    double *polynome = new double[degree + 1];
    printf("... and coefficients: ");
    for (int i = 0; i <= degree; i++)
        scanf("%lf", polynome + i);

    printPolynomeSmartPrecision(polynome, degree + 1, topRow, bottomRow);
    printf("%s\n%s\n", topRow, bottomRow);

    return 0;
}
