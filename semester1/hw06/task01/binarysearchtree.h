#pragma once
#include <cstdlib>

/* Самому пока не очень, прямо скажем, нравится все то, что здесь происходит. В целом идеи следующие:
  1. Камнем предкновения оказалась процедура удаления элемента из дерева. Проблем несколько.
     Во-первых, для удаления нужен родитель. Вводить наравне с функцией поиска функцию getParent не очень
     хочется, как и, в особенности, не хочется вносить в код явную двойную работу - чтобы найти родителя
     нужно заглянуть в того ребенка, где должен быть искомый ключ - а чтобы потом использовать этого родителя
     для операции удаления нужно снова будет в другой уже функции сделать тоже самое. Так что мой search возвращает
     специальную структурку, содержащую ссылку на то поле родителя узла, содержащего или должного содержать искомый
     ключ, которое содержит указатель на сам этот узел - или NULL если его нет, а также структурка содержит указатель
     на самого родителя искомого узла
     Во-вторых, непонятно, как удалять корень дерева. Так что пока пришел к необходимости иметь отдельные классы для
     узла дерева и для него самого.
  2. search и insert получились действительно аккуратными и лаконичными - и без лишних действий по ходу работы.
     Попытка перенести работу со структурой BinarySearchResult на остальные функции дала достаточно неэлегантный код
     remove.
  3. Наличие отдельного класса дерева подтолкнуло к попыткам использовать своеобразное "прицеливание" нового дерева
     в узел существующего с целью использовать методы такого временного дерева для осуществления локальных операций.
     Не очень гладко эту идею удалось реализовать.
*/

struct BinarySearchNode
{
    int m_key;
    BinarySearchNode *m_leftChild;
    BinarySearchNode *m_rightChild;

    BinarySearchNode(int key) :
        m_key(key), m_leftChild(NULL), m_rightChild(NULL) {}

    ~BinarySearchNode();
};

struct BinarySearchResult
{
    BinarySearchNode *&m_node;
    BinarySearchNode *m_parent;

    BinarySearchResult(BinarySearchNode *&place, BinarySearchNode *parent) :
        m_node(place), m_parent(parent) {}

    bool operator ==(BinarySearchNode *right) const { return right == m_node; }
    bool operator !=(BinarySearchNode *right) const { return right != m_node; }
};

class BinarySearchTree
{
public:
    BinarySearchTree(BinarySearchNode *root = NULL) : m_root(root) {}
    BinarySearchTree(const int *array, int size);
    ~BinarySearchTree();

    bool isEmpty() const { return m_root == NULL; }
    bool hasKey(int key) { return search(key) != NULL; }
    int successor(int key) const { return successorNode(key)->m_key; }  // (!) Не реализовано
    int predecessor(int key) const { return predecessorNode(key)->m_key; }  // (!) Не реализовано
    int min() { return minNode().m_node->m_key; }
    int max() { return maxNode().m_node->m_key; }

    virtual void insert(int key);
    virtual void remove(int key);

    void symorder(void (*act)(int key));
    void symorderBack(void (*act)(int key));

protected:
    BinarySearchResult search(int key);

    BinarySearchNode *successorNode(int key) const;  // (!) Не реализовано
    BinarySearchNode *predecessorNode(int key) const;  // (!) Не реализовано
    BinarySearchResult minNode();
    BinarySearchResult maxNode();

    BinarySearchNode *m_root;
};
