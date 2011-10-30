#pragma once
#include <iostream>

/* Класс, имеющий хотя бы одну чистую (зануленную) виртуальную функцию является абстрактным - невозможно создать его
   экземпляры. Абстрактный класс, будучи базовым для наследников, реализует один из принципов ООП - полиморфизм,
   кратко который можно определить как возможность существования объектов, которые выглядят похоже, делают что-то
   похожее - но делают это каждый по-своему */

class TreeNode
/**
 * Абстрактный базовый класс-интерфейс узлов дерева - обеспечивает необходимую иерархию классов. Объективно необходим
 * 1. для введения в систему типа унифицированного указателя и на листья дерева, содержащего целочисленные константы,
 *    и на иные его узлы (поддеревья), содержащие знаки операций.
 * 2. для обеспечения единого интерфейса вышеописанных классов, значит - неразличимого для клиента классов поведения
 *    разнородных по своему содержанию узлов дерева.
 * 3. для создания иерархии деструкторов и обеспечения удаления дерева из динамической памяти по типу цепной реакции.
 */
{
public:
    virtual int calculate() const = 0;                // Зануленный виртуальный метод вынуждает всякого наследника
    virtual void print(std::ostream &out) const = 0;  // ... переопределить такой метод
    virtual ~TreeNode() {}                            // Обычный виртуальный метод позволяет наследнику по желанию
                                                      // ... переопределить его реализацию, в данном случае inline'-
// Ситуация с виртуальным деструктором не совсем      // ... овую (обратите внимание на {})
// ... типичная - он не столько переопределяется
// ... наследниками, сколько входит в сцепку с
// ... их деструкторами, образуя иерархию - при
// ... вызове дестуктора базового класса реально
// ... будет вызван и он, и деструктор именно того
// ... класса, к которому принадлежит объект.
};

/* Чтобы расширить свой класс можно унаследоваться от другого класса, позаимствовав его интерфейс и/или реализацию.
   Существует три режима наследования:
   public -    сохраняет содержимое секций public и protected базового класса как есть (напомню, что секция private
               не доступна для наследования) Только такое наследование (!) разрешает неявное преобразование указателя
               на объект класса-потомка в указатель на базовый класс.
   protected - наследует содержимое public и protected в protected
   private   - -//- в private */

class LeafNode : public TreeNode
/**
 * Класс-лист дерева. Его задача - хранить целочисленную константу-аргумент и при этом выглядеть точно так же, как и
 * любое более сложное поддерево - уметь отвечать на запрос .calculate() так, будто он и правда что-то считает.
 */
{
public:
    LeafNode(std::istream &in) { in >> m_value; }                  // Конструктор считывает в лист int
    int calculate() const { return m_value; }                      // Все расчеты заключаются в выдаче хранимого int
    void print(std::ostream &out) const { out << ' ' << m_value; }

private:                      // Обратите внимание - удалять тут нечего, потому
    int m_value;              // ... и деструктор базового класса не переопределен
};

class OperationNode : public TreeNode
/**
 * Класс-поддерево с операцией. Является формообразующим элементом дерева, поскольку хранит указатели на левого и
 * правого сына. Также, по сути, является конструктором всего итогового арифметического дерева.
 */
{
public:
    OperationNode(std::istream &in);
    ~OperationNode();

    int calculate() const;
    void print(std::ostream &out) const;

protected:
    TreeNode *getTreeNode(std::istream &in);                       // Внутренние, служебные методы лучше спрятать в
                                                                   // ... наследуемую protected-секцию
private:
    char m_operation;
    const TreeNode *m_left;   // На деле такие указатели могут хранить адреса объектов любых классов, являющихся
    const TreeNode *m_right;  // ... public-наследниками класса TreeNode - что нам и нужно, так как одним из сыновей
                              // ... может оказаться лист с константой, а другим - поддерево с операцией

/* Подробные комментарии по поводу друзей см. здесь: https://gist.github.com/1322950 */
friend std::ostream &operator <<(std::ostream &out, const OperationNode &self);
};