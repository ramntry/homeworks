// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 01, task 04

// Посчитать число "счастливых билетов" (билет считается
// "счастливым", если сумма первых трёх цифр
// его номера равна сумме трёх последних)

// [time start] 19:11 17.09.11
// [time estimate] 00:25 00


#include <iostream>

using namespace std;

int main()
{
    int counter[28];
    for (int i = 0; i < 28; i++)
        counter[i] = 0;

    for (int i = 0; i < 10; i++)
        for (int j = 0; j < 10; j++)
            for (int k = 0; k < 10; k++)
                ++counter[i + j + k];

    int result = 0;
    for (int i = 0; i < 28; i++)
        result += counter[i] * counter[i];

    cout << "The number of happy ticket = "
         << result
         << endl;

    return 0;
}


// [time done] 19:28 17.09.11
// [time real] 00:17 00

