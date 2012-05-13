#pragma once
#include <QtCore/Qt>
#include <iostream>
#include <cmath>
#include <utility>
#include "../../hw02/hw02_task02/stack/dynamicstack.h"
#include "../../hw03/hw03_task01/comparator.h"

/**
 * Шаблонная реализация АТД "Мультимножество", использующая реальное хранение экземпляров множественных объектов,
 * на основе декартового дерева.
 */
template <typename T, typename Cmp = StandartComparator<T> >
class Bag
{
public:
    class Iterator;          /** Итератор по коллекции, в качестве которой может выступать все мультимножество или
                                 его часть - коллекция экземпляров равных данному. Гарантируется итерация в порядке
                                 возрастания элементов. Не гарантируется порядок итерации по равным элементам, равный
                                 порядку их добавления во множество. Итератор допускает операции разыменовывания (*it)
                                 и преинкремента (++it). Признаком конца коллекции служит устанавливающееся равенство
                                 со специальным end-итератором, экземляр которого может быть получен с помощью
                                 статического фабричного метода Bag<int>::end(). Для определения равенства возможно
                                 использование операторов (==) и (!=) */
    Bag();
    ~Bag();

    void add(T const& value);       /** Добавление элемента в множество */
    void remove(T const& value);    /** Удаление одного экземпляра, равного данному, если такой есть */
    void erase(T const& value);     /** Удаление всех экземпляров, равных данному */

    bool has(T const& value)        /** Проверка существования во множестве хотя бы одного экземпляра, равного данному */
    { return search(mRoot, value) != NULL; }

    size_t size()                   /** Текущее число неуникальных элементов во множестве */
    { return mSize; }

    Iterator find(T const& value);  /** Поиск коллекции экземпляров, равных данному. Возвращает итератор типа
                                        Bag<T, Cmp>::Iterator */
    Iterator begin();               /** Возвращает итератор на первый элемент мультимножества. */

    static Iterator end();   /** Возвращает специальный end-итератор */

protected:
    class AbstractIterator;  /** Интерфейсный класс, обеспечивающий делегирование итератором типа Iterator его методов
                                 конкретной реализации итератора нужного типа */
    class TraversalIterator; /** Реализация итератора обхода */
    class FindIterator;      /** Реализация поискового итератора */

    /** Узел декартового дерева */
    struct TreapNode
    {
        TreapNode(T const& value)
            : leftChild(NULL)
            , rightChild(NULL)
            , priority(qrand())
            , key(value)
        {}

        TreapNode *leftChild;
        TreapNode *rightChild;

        int priority;
        T key;
    };

    typedef std::pair<TreapNode *, TreapNode *> Parts;

    /** Возвращает части дерева с корнем в rootNode, такие, что все элементы левой части (Parts::first) строго меньше
        (либо меньше или равны при strict == false) данного ключа key, а элементы правой (Parts::second) строго больше
        (либо больше или равны при strict == true) */
    Parts split(TreapNode *rootNode, T key, bool strict = false);

    /** Сливает два дерева (такие, как Parts) в одно */
    TreapNode *merge(TreapNode *left, TreapNode *right);

    /** Осуществляет вставку экземпляра newNode в коллекцию ему равных экземпляров list, представляющую собой
        вырожденное в список левых дерево, сохраняя инвариант list - всякий элемент коллекции является левым сыном
        своего родителя, если тот существует */
    void insert(TreapNode *newNode, TreapNode *&list);

    /** Симметрический обход дерева с выполнением function над каждым его узлом. Используется в деструкторе и в выводе
        ключей дерева в поток в порядке возрастания */
    template <typename F>
    void traversal(TreapNode *rootNode, F function);

    /** Возращает указатель на ближайший к root узел дерева, ключ которого равен value, либо NULL если такой не найден */
    static TreapNode *search(TreapNode *root, T value);

private:
    TreapNode *mRoot;
    size_t mSize;

    static Cmp cmp;

template <typename E, typename C>
friend std::ostream &operator <<(std::ostream &os, Bag<E, C> &bag);

template <typename E, typename C, typename S>
friend Bag<E, C> &operator <<(Bag<E, C> &bag, S item); // псевдоним add
};

template <typename T, typename Cmp>
Cmp Bag<T, Cmp>::cmp = *(new(&Bag<T, Cmp>::cmp) Cmp);  /// PROBLEM

//////////////////////////////////////   Bag   //////////////////////////////////////

template <typename T, typename Cmp>
Bag<T, Cmp>::Bag()
    : mRoot(NULL)
    , mSize(0)
{
}

template <typename T, typename Cmp>
Bag<T, Cmp>::~Bag()
{
    traversal(mRoot, [](TreapNode *node)
    {
        delete node;
    });
}

template <typename T, typename Cmp>
typename Bag<T, Cmp>::Parts Bag<T, Cmp>::split(Bag<T, Cmp>::TreapNode *rootNode, T key, bool strict)
{
    if (rootNode == NULL)
        return Parts(NULL, NULL);

    if ((cmp)(key, rootNode->key) > 0|| (!strict && (cmp)(key, rootNode->key) == 0))
    {                                                             // если разрез проходит правее корня дерева, то
        Parts parts = split(rootNode->rightChild, key, strict);   // нужно рекурсивно рассечь правое поддерево. Тогда
        rootNode->rightChild = parts.first;                       // левая часть - это бывший корень с неизменным
        return Parts(rootNode, parts.second);                     // левым поддеревом и правым, равным левой части
    }                                                             // правого поддерева, а правая часть - это правая
    else                                                          // часть правого поддерева.
    {
        Parts parts = split(rootNode->leftChild, key, strict);    // иначе - симметрично
        rootNode->leftChild = parts.second;
        return Parts(parts.first, rootNode);
    }
}

template <typename T, typename Cmp>
typename Bag<T, Cmp>::TreapNode *Bag<T, Cmp>::merge(Bag<T, Cmp>::TreapNode *left, Bag<T, Cmp>::TreapNode *right)
{
    if (left == NULL)    // Слияние с пустым деревом не меняет второе из сливаемых
        return right;
    if (right == NULL)
        return left;

    if (left->priority > right->priority ||                // если приоритет корня левой части выше приоритета корня
        (left->priority == right->priority && (cmp)(left->key, right->key) < 0)) // правой, то корень левой - новый корень с
    {                                                      // с неизменным левым поддеревом и объединением правой
        left->rightChild = merge(left->rightChild, right); // части и правого поддерева - в качестве правого поддерева
        return left;
    }
    else                                                   // иначе - симметрично
    {
        right->leftChild = merge(left, right->leftChild);
        return right;
    }
}

template <typename T, typename Cmp>
void Bag<T, Cmp>::insert(TreapNode *newNode, Bag<T, Cmp>::TreapNode *&list)
{
    if (list == NULL || list->priority < newNode->priority) // ... то новый элемент - голова списка
    {
        newNode->leftChild = list;
        list = newNode;
    }
    else // ... необходимо осуществить вставку ниже по списку
        insert(newNode, list->leftChild);
}

template <typename T, typename Cmp>
void Bag<T, Cmp>::add(const T &value)                                // рассечем дерево так, чтобы одна часть (strict.first)
{                                                        // содержала элементы строго меньшие добавляемого, другая
    Parts nonStrict = split(mRoot, value);               // (strict.second) равные ему и имела вид списка левых,
    Parts strict = split(nonStrict.first, value, true);  // а третья (nonStrict.second) состояла из элементов, строго
                                                         // больших добавляемого.
    insert(new TreapNode(value), strict.second);         // добавим новый элемент в коллекцию равных ему

    TreapNode *tmp = merge(strict.first, strict.second); // восстановим целостное дерево
    mRoot = merge(tmp, nonStrict.second);
    mSize++;
}

template <typename E, typename C, typename S>
Bag<E, C> &operator <<(Bag<E, C> &bag, S item)
{
    bag.add(E(item));
    return bag;
}

template <typename T, typename Cmp>
void Bag<T, Cmp>::remove(const T &value)
{
    Parts nonStrict = split(mRoot, value);               // получим коллекцию элементов, равных данному
    Parts strict = split(nonStrict.first, value, true);
    TreapNode *toDel = strict.second;

    if (toDel != NULL)                                   // если список элементов, равных данному не пуст
    {
        strict.second = strict.second->leftChild;        // удалим голову этого списка
        delete toDel;
        mSize--;
    }

    TreapNode *tmp = merge(strict.first, strict.second); // восстановим целостное дерево
    mRoot = merge(tmp, nonStrict.second);
}

template <typename T, typename Cmp>
void Bag<T, Cmp>::erase(const T &value)
{
    Parts nonStrict = split(mRoot, value);
    Parts strict = split(nonStrict.first, value, true);

    traversal(strict.second, [&mSize](TreapNode *node)   // то же, только удалим весь список равных данному
    {
        delete node;
        mSize--;
    });

    mRoot = merge(strict.first, nonStrict.second);
}

template <typename T, typename Cmp>
typename Bag<T, Cmp>::TreapNode *Bag<T, Cmp>::search(Bag<T, Cmp>::TreapNode *cursor, T value)
{
    while (cursor != NULL && (cmp)(cursor->key, value) != 0)
        if ((cmp)(value, cursor->key) < 0)
            cursor = cursor->leftChild;
        else
            cursor = cursor->rightChild;

    return cursor;
}

template <typename T, typename Cmp>
template <typename F>
void Bag<T, Cmp>::traversal(TreapNode *rootNode, F function)
{
    if (rootNode == NULL)
        return;

    traversal(rootNode->leftChild, function);
    TreapNode *rightChild = rootNode->rightChild;
    function(rootNode);
    traversal(rightChild, function);
}

template <typename E, typename C>
std::ostream &operator <<(std::ostream &os, Bag<E, C> &bag)
{
    os << "Bag( ";
    bag.traversal(bag.mRoot, [&os](typename Bag<E, C>::TreapNode *node)
    {
        os << node->key << ' ';
    });

    return os << ')';
}


///////////////////////////////////   Iterators   ///////////////////////////////////

template <typename T, typename Cmp>
class Bag<T, Cmp>::AbstractIterator
{
public:
    AbstractIterator(TreapNode *root = NULL)
        : cursor(root)
    {}

    virtual ~AbstractIterator() {}

    T &get()                    // для (*it)
    { return cursor->key; }

    virtual void next() = 0;    // для (++it)
    virtual bool hasNext() = 0; // для (==) и (!=) c end-итератором

protected:
    TreapNode *cursor;          /** Текущая позиция итератора */

friend class Bag<T, Cmp>::Iterator;  // для доступа к cursor (необходимо для корректного сравнения не end-итераторов)
};

template <typename T, typename Cmp>
class Bag<T, Cmp>::Iterator
{
public:
    Iterator()     /** Через интерфейс класса Bag доступен только конструктор, создающий end-итератор */
        : d(NULL)
        , thisIsCopy(false)
    {}

    Iterator(Iterator const& src)
        : d(src.d)
        , thisIsCopy(true)
    {}

    ~Iterator()
    { if (!thisIsCopy) delete d; }  /** Iterator самостоятельно освобождает память, занятую конкретной реализацией */

    T &operator *()
    { return d->get(); }

    void operator ++()
    { d->next(); }

    bool operator !=(Iterator that)
    { return !(*this == that); }

    bool operator ==(Iterator r);

protected:
    Iterator(Bag<T, Cmp>::AbstractIterator *impl) // итератор, отличный от end-итератора создается публичными фабричными
        : d(impl)                            // методами класса Bag, например, find(T const& value) и begin()
        , thisIsCopy(false)                  // обеспечивает единственность и корректность реализации копий итератора
    {}

    AbstractIterator *d; // указатель на реализацию
    bool thisIsCopy;

friend class Bag;        // для доступа фабричныx методов Bag к защищенному конструктору AbstractIterator
};

template <typename T, typename Cmp>
bool Bag<T, Cmp>::Iterator::operator ==(typename Bag<T, Cmp>::Iterator r)
{
    if (r.d == NULL)                         // end-итератору, созданному Bag::end() равны
        return d == NULL || !d->hasNext();   // аналогичный, либо достигнувший конца коллекции
    if (d == NULL)
        return r.d == NULL || !r.d->hasNext();

    return d->cursor == r.d->cursor;         // два не end-итератора равны только если указывают на один и тот же
}                                            // участок памяти. Два итератора, достигнувшие концов различных
                                             // различных коллекций, различны.

/** Итератор обхода */
template <typename T, typename Cmp>
class Bag<T, Cmp>::TraversalIterator : public Bag<T, Cmp>::AbstractIterator
{
public:
    TraversalIterator(size_t size, TreapNode *root);
    void next();

    bool hasNext()
    { return counter > 0; }

protected:
    typedef Bag<T, Cmp>::AbstractIterator super;

    void up();    /** Поднимает cursor до первого шага вверх и вправо */
    void down();  /** Утапливае cursor на шаг вниз и вправо и далее влево до упора */

    DynamicStack<TreapNode *> stack;  /** Служит для хранения родительских вершин */
    int counter;  /** Счетчик числа непройденных элементов. Используется в hasNext */
};

template <typename T, typename Cmp>
Bag<T, Cmp>::TraversalIterator::TraversalIterator(size_t size, TreapNode *root)
    : AbstractIterator(root)
    // Декартово дерево обеспечивает с вероятностью, стремящейся к 1, что высота дерева не будет превышать
    // 4 * log_2(size) ~= 6 * log_e(size). На практике она в среднем оказывается равной 3.5 * log_e(size).
    // В случае мультимножества реальная высота дерева оказывается равной max(3.5 * log_e(size), N), где N -
    // размер наибольшей коллекции равных элементов в мультимножестве. В связи с малой величиной log(size) и заранее
    // не предвидимым, но потенциально большим значением N выбраны следующие значения capacity и multiplier для стека:
    , stack(std::min(size, (size_t)(4 * log(size + 1))), 3.0)
    , counter(size)
{
    while (super::cursor->leftChild) // инициализация итератора состоит в спуске до минимального элемента дерева
    {
        stack.push(super::cursor);
        super::cursor = super::cursor->leftChild;
    }
}

template <typename T, typename Cmp>
void Bag<T, Cmp>::TraversalIterator::next()
{
    if (--counter <= 0) // охранник, предотвращающий попытки извлечения элемента из пустого стека
        return;

    if (super::cursor->rightChild != NULL) // если есть возможность пройти вправо и вниз, идем
        down();
    else                                   // иначе всплываем
        up();
}

template <typename T, typename Cmp>
void Bag<T, Cmp>::TraversalIterator::up()
{
    while (stack.look()->rightChild == super::cursor) // пока движение вверх - это движение влево
        super::cursor = stack.pop();                  // всплываем

    super::cursor = stack.pop();                      // шаг в еще необойденную родительскую вершину
}

template <typename T, typename Cmp>
void Bag<T, Cmp>::TraversalIterator::down()
{
    super::cursor = super::cursor->rightChild;        // как только был обойден левый сын и корень, делаем шаг вправо
    while (super::cursor->leftChild != NULL)          // где обойдем сначала все то, что слева
    {
        stack.push(super::cursor);
        super::cursor = super::cursor->leftChild;
    }
}

/** Поисковый итератор */
template <typename T, typename Cmp>
class Bag<T, Cmp>::FindIterator : public Bag<T, Cmp>::AbstractIterator
{
protected:
    typedef Bag<T, Cmp>::AbstractIterator super;

public:
    FindIterator(TreapNode *root) // Поисковый итератор инициализируется фабричным методом, устанавливающим начальное
        : AbstractIterator(root)  // положение курсора в позицию ближайшего к корню дерева вхождения искомого элемента
    {}

    void next();

    bool hasNext()
    { return super::cursor != NULL; }
};

template <typename T, typename Cmp>
void Bag<T, Cmp>::FindIterator::next()
{
    if (super::cursor == NULL)
        return;

    T value = super::cursor->key;
    super::cursor = super::cursor->leftChild;     // Элементы, равные данному, хранятся только в левом поддереве данного
    super::cursor = search(super::cursor, value);
}


//////////////////////////////////////   Bag   //////////////////////////////////////
// Фабричные методы, возвращающие итераторы

template <typename T, typename Cmp>
typename Bag<T, Cmp>::Iterator Bag<T, Cmp>::end()
{
    return Iterator();
}

template <typename T, typename Cmp>
typename Bag<T, Cmp>::Iterator Bag<T, Cmp>::begin()
{
    if (mSize == 0)
        return Iterator();

    return Iterator(new TraversalIterator(mSize, mRoot));
}

template <typename T, typename Cmp>
typename Bag<T, Cmp>::Iterator Bag<T, Cmp>::find(const T &value)
{
    return Iterator(new FindIterator(search(mRoot, value)));
}


/////////////////////////////////////   Tests   /////////////////////////////////////

int bagTestExec(int argc, char **argv);
