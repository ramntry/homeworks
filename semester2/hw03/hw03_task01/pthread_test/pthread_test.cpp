#include <ctime>
#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <algorithm>
#include <pthread.h>
#include "../BubbleSorter.h"
#include "../HeapSorter.h"

// Объект-таймер времени работы функций.
struct Timer
{
    Timer(const char *_name)
        : name(_name)
    {
        printf("%s started...\n", name);
        start = clock();
    }

    ~Timer()
    {
        clock_t finish = clock();
        printf("%s: %.2lf seconds\n\n\n",
                name, (double)(finish - start) / CLOCKS_PER_SEC);
    }

    const char *name;
    clock_t start;
};

// Генератор массива псевдослучайных чисел заданного размера.
// Последовательность уникальна от запуска программы к запуску,
// но строго одна и та же для последовательных вызовов createArray
// в рамках одного запуска.
int *createArray(size_t size)
{
    static time_t srandBase;
    static bool   isFirstCall = true;

    if (isFirstCall)
    {
        time(&srandBase);
        isFirstCall = false;
    }
    srand(srandBase);

    int *array = new int[size];
    for (size_t i = 0; i < size; ++i)
        array[i] = rand();

    return array;
}


// Обертка STL-версии алгоритма сортировки.
SORTER_METHOD(SystemSorter)
{
    std::sort(data, data + size);
}

// Двухпоточная обертка STL-версии алгоритма сортировки.
SORTER_METHOD(MultiThreadSystemSorter)
{
    // Специализированная на сортировку std::sort'ом обертка POSIX-потока.
    class ThreadSorter
    {
        pthread_t thread;
        T *iterators[2]; // указатели на начало и конец сортируемого массива

        // поток
        static void *threadRoutine (void *iterators)
        {
            T **its = (T **)iterators;
            printf("start sort in second thread... "
                    "(from %p to %p, size: %ld)\n", its[0], its[1], its[1] - its[0]);

            std::sort(its[0], its[1]);

            printf("sort in second thread is done\n");
            return NULL;
        }

    public:
        // запуск потока на сортировку
        void run(T *begin, T *end)
        {
            iterators[0] = begin;
            iterators[1] = end;
            pthread_create(&thread, NULL, &threadRoutine, iterators);
        }

        // ожидание завершения потока
        void join()
        {
            pthread_join(thread, NULL);
        }
    };

    // весь исходный массив будет скопирован в отдельную область
    // памяти, отсортирован по кускам в основном и дополнительном потоках
    // и слит стандартным std::merge в исходный массив.
    T *tmp = new T[size];
    T *middle = tmp + size / 2;  // граница между левым и правым кусками
    T *end = tmp + size;

    for (size_t i = 0; i < size; ++i)
        tmp[i] = data[i];

    ThreadSorter threadSorter;

    printf("\nstart thread...\n");
    threadSorter.run(tmp, middle);
    printf("thread started\n");

    printf("start sort in main thread... "
            "(from %p to %p, size: %ld)\n", middle, end, end - middle);
    std::sort(middle, end);

    printf("sort in main thread is done. Join to second thread...\n");
    threadSorter.join();
    printf("second thread is done\n");

    printf("start merge results...\n");
    std::merge(tmp, middle, middle, end, data);
    printf("merge results is done\n\n");

    delete[] tmp;
}

// !!! Освобождает память по переданному указателю array
void test(const char *name, Sorter<int> *sorter, int *array, size_t size)
{
    Timer timer(name);
    printf("array size: %lu\n"
            "first: %10d; last: %10d\n", size, array[0], array[size - 1]);

    sorter->sort(array, size);

    printf("first: %10d; last: %10d\n", array[0], array[size - 1]);
    delete[] array;
}

void pthreadTest()
{
    size_t taskSize = 0;
    printf("task size: ");
    scanf("%lu", &taskSize);
    putchar('\n');
    // Для компенсации различий в работе квадратичных и более адекватных
    // алгоритмов для последних увеличим размер задачи так, чтобы приблизительно
    // сравнять время работы этих двух групп алгоритмов сортировки.
    size_t nlognTaskSize = taskSize * pow(10, floor(log(log(taskSize))));

    Sorter<int> *bubble  = new BubbleSorter<int>;
    Sorter<int> *heap    = new HeapSorter  <int>;
    Sorter<int> *sys     = new SystemSorter<int>;
    Sorter<int> *mthread = new MultiThreadSystemSorter<int>;

//    test("BubbleSorter", bubble,  createArray(taskSize), taskSize);
//    test("HeapSorter",   heap,    createArray(nlognTaskSize), nlognTaskSize);
    test("SystemSorter", sys,     createArray(nlognTaskSize), nlognTaskSize);
    test("MultiThreadSystemSorter",
                         mthread, createArray(nlognTaskSize), nlognTaskSize);

    delete bubble;
    delete heap;
    delete sys;
    delete mthread;
}
