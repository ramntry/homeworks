// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 01, task 02, v2

// Реализовать алгоритм нахождения неполного частного от
// деления a на b (целые числа), используя только
// операции сложения, вычитания и умножения.

// [time start] 17:23 17.09.11
// [time estimate] 00:10 00


#include <iostream>
#include <cstdlib>

using namespace std;

int main()
{
    int a = 0;
	int b = 0;
	int incompQuot = 0;
    bool isNegative = false;

    cout << "This program calculates the incomplete quotient"
         << " of a and b\nEnter\ta b: ";
    cin  >> a >> b;

    if (b == 0)  // 0 / 0 - любое число, но эта ситуация не учитывается.
	{
        cout << "Zero division error" << endl;
        return 1;
    }
	
    if (a * b < 0)          // Для отрицательных аргументов дублируется
        isNegative = true;  // поведение C++ (GCC 4.6)
    b = abs(b);
    a = abs(a) - b;
    while (a >= 0)
	{
    	a -= b;
        incompQuot++;
	}
    if (isNegative)
        incompQuot *= -1;

    cout << "a / b = " << incompQuot << endl;

    return 0;
}

// [time done] 18:12 17.09.11
// [time real] 00:49 00

