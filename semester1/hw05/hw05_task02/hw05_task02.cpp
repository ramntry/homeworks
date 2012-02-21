#include <stdio.h>

typedef unsigned long long llong;

/// Для разрешения побитовых операций и устранения зависимости
/// от порядка байт, а также хранения промежуточных значений

struct doublePrep
{
    union
    {
        double value;
        llong bits;
    };
    llong mant;
    int exp;
    char sign;
};


/// Расчет мантиссы с использованием только целочисленной арифметики
/// (мантисса вида 1xxx... где точка между 1 и остальной частью отсутствует,
/// но подразумевается)

doublePrep calcMant(doublePrep n)
{
    llong mask = 1;
    mask <<= 63;
    n.sign = mask & n.bits ? '-' : '+';   // получение знака
    mask >>= 12;        // установка маски под первый бит мантиссы

    llong toNext = 0;        // & с маской toNext определяет,
    toNext = ~toNext;        // есть ли еще значащие биты
    llong max = toNext / 10; // для защиты от переполнения
    toNext >>= 12;           // ... llong mant

    n.mant = 1;
    llong p5 = 1;   // 10 / 2 = 5 (отношение систем исчисления)

    while (toNext & n.bits)  // пока есть значащие биты в мантиссе
    {
        if (n.mant <= max)   // и нет угрозы переполнения
        {
            n.mant *= 10; // резервируем место в записи мантиссы
            p5 *= 5;      // и "делим" текущее значение позиции на 2
        }
        else              // иначе делим напрямую (при больших значениях
            p5 /= 2;      // ... ощутимо поднимает точность - иначе неважно)
                             // добавляем к мантиссе дробь вида 1 / 2^i,
        if (n.bits & mask)   // где i - позиция бита в двоичной мантиссе, 
            n.mant += p5;    // только если бит установлен 

        mask >>= 1;
        toNext >>=1;
    }
    return n;
}


// расчет порядка экспоненты по основанию 2

doublePrep calcExp(doublePrep n)
{
    llong lbits = n.bits;
    lbits <<= 1;    // сброс бита знака
    lbits >>= 53;   // - / - мантиссы
    n.exp = lbits;  // - / - ограничений на unsigned
    n.exp -= 1023;  // - / - смещения

    return n;
}

/// вспомогательный инструмент - подсчет числа
/// цифр в десятичной записи числа
/// (log10 не используется по идеологическим соображениям)

int numDigits(llong n)
{
    int i = 0;
    while (n)
    {
        n /= 10;
        i++;
    }
    
    return i;
}

/// перевод основания стандартной записи числа от 2 к 10

doublePrep exp2exp10(doublePrep n)
{
    llong mask = 1;
    mask <<= 63;

    if (n.exp > 0)  // подходы крайне разные. Пусть exp отрицательна
    {
        int exp10 = 1 - numDigits(n.mant);  // Например:
        while (n.exp)   // ... 24: из формы 1.5*2^4 получим 15*2^4 * 10^-1
        {   // ... а затем в цикле "перегоним" exp по 2 в exp по 10
            if (n.mant & mask)  // если есть угроза переполнения при * 2
            {
                n.mant /= 10;
                exp10++;
            }
            n.mant <<= 1;
            n.exp--;
        }
        exp10 += numDigits(n.mant) - 1;
        n.exp = exp10;
    }
    else if (n.exp < 0)
    {
        int exp10 = 0;
        while (n.mant * 10 / 10 == n.mant) // пока нет угрозы переполнения
            n.mant *= 10;   // максимально увеличим точность расчетов
        n.mant /= 10;
        llong max = n.mant / 10;

        while (n.exp)
        {
            n.mant >>= 1;  // избавляемся от exp по 2
            if (n.mant * 10 / 10 == n.mant && n.mant > max)  // если по ходу появилась
            {              // возможность нарастить exp по 10 - делаем это
                n.mant *= 10;
                exp10--;
            }
            n.exp++;
        }
        n.exp = exp10;
    }

    return n;
}

/// округление мантиссы до 15-16 знаков - далее мусор

doublePrep round16(doublePrep n)
{
    int lastNum = 0;
    llong max = 1e16;
    if (n.exp < 0)
        max = 1e15;
    while (n.mant > max)
    {
        lastNum = n.mant % 10;
        n.mant /= 10;
    }

    if (lastNum > 4)  // округляем по математическим правилам
            n.mant++;

    while (n.mant % 10 == 0)
        n.mant /= 10;

    return n;
}

int main(void)
{
    printf("Representation of a real number in standard form with the %s",
           "exponent to base 10 (send EOF (ctrl + D) to exit)\n");

    printf("d = ");
    doublePrep n;
    while(scanf("%lf", &n.value) != EOF)
    {
        if (!(n.bits << 1))     // не будем различать +0 и -0
        {
            printf("\t 0.0\nd = ");
            continue;
        }

        n = calcMant(n);
        n = calcExp(n);
        n = exp2exp10(n);
        n = round16(n);
        
        char buf[17];  // пляска ради того, чтобы отобразить точку между
        sprintf(buf, "%llu", n.mant); // ... первой значащей цифрой мантиссы
                                      // ... и последующими
        printf("\t%c%c.%se%+d\nd = ",
                n.sign, buf[0], buf[1] ? buf + 1 : "0", n.exp); 
    }
    putchar('\n');

    return 0;
}

