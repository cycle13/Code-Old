---
layout:     post
title:      "C++ 泛型编程基础：模板通识"
subtitle:   "C++ generic programming basics"
date:       2017-08-20
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - C/C++
---

> 测试环境：Target: x86_64-linux-gnu <br/>
> gcc version 5.3.1 20160413 (Ubuntu 5.3.1-14ubuntu2.1)

什么是泛型编程？为什么C++会有模板？这一切的一切都要从如何编写一个通用的加法函数说起。

# 很久很久以前

有一个人要编写一个通用的加法函数，他想到了这么几种方法：

* 使用函数重载,针对每个所需相同行为的不同类型重新实现它

```
int Add(const int &_iLeft, const int &_iRight)
{
return (_iLeft + _iRight);
}
float Add(const float &_fLeft, const float &_fRight)
{
return (_fLeft + _fRight);
}
```

当然不可避免的有自己的缺点：

1. 只要有新类型出现,就要重新添加对应函数，太麻烦
2. 代码的复用率低
3. 如果函数只是返回值类型不同,函数重载不能解决（函数重载的条件：同一作用域，函数名相同，参数列表不同）
4. 一个方法有问题,所有的方法都有问题,不好维护

* 使用公共基类,将通用的代码放在公共的基础类里面

【缺点】

1. 借助公共基类来编写通用代码,将失去类型检查的优点
2. 对于以后实现的许多类,都必须继承自某个特定的基类,代码维护更加困难

* 宏

```
#define ADD(a, b) ((a) + (b))
```

1. 不进行参数类型检测,安全性不高
2. 编译预处理阶段完成替换，调试不便

所以在C++中又引入了泛型编程的概念。泛型编程是编写与类型无关的代码。这是代码复用的一种手段。模板则是泛型编程的基础。

模板分为了函数模板和类模板：

![img](/img/in-post/2017-08-20-C++-generic-programming-basics/01.png)

# 函数模板

函数模板:代表了一个函数家族,该函数与类型无关,在使用时被参数化,根据实参类型产生函数的特定类型版本。

什么意思呢？往下看就知道了。

## 模板函数的格式

```
template<typename Param1, typename Param2,…,class Paramn>
返回值类型 函数名(参数列表)
{
…
}
```

一个简单的Add函数模板：

```
template <typename T> //T可是自己起的名字，满足命名规范即可  
T Add(T left, T right)  
{  
    return left + right;  
}  
  
int main()  
{  
    Add(1, 2); //right 调用此函数是将  
    Add(1, 2.0); //error 只有一个类型T，传递两个不同类型参数则无法确定模板参数T的类型，编译报错  
    return 0;  
}
```

![img](/img/in-post/2017-08-20-C++-generic-programming-basics/02.png)

对第一个函数调用，编译器生成了 int Add(int, int) 这样一个函数。

typename是用来定义模板参数关键字,也可以使用class。不过建议还是尽量使用typename，因为这个关键字是为模板而生的！

注意:不能使用struct代替typename。（这里的class不是之前那个class的意思了，所以你懂的）

当然你也可以把函数模板声明为内联的：

```
template <typename T>
inline T Add(T left, T right) {//…}
```

# 实例化

编译器用模板产生指定的类或者函数的特定类型版本,产生模板特定类型的过程称为函数模板实例化。(用类类型来创建一个对象也叫做实例化哦！)

# 模板的编译

模板被编译了两次:

1. 实例化之前,检查模板代码本身,查看是否出现语法错误,如:遗漏分号（遗憾的是不一定能给检查的出来）
2. 在实例化期间,检查模板代码,查看是否所有的调用都有效,如:实例化类型不支持某些函数调用

实参推演

从函数实参确定模板形参类型和值的过程称为模板实参推演。多个类型形参的实参必须完全匹配。

如对这样的函数调用：

```
template <typename T1, typename T2, typename T3>  
void fun(T1 t1, T2 t2, T3 t3)  
{  
    //do something  
}  
int main()  
{  
    fun(1, 'a', 3.14);  
    return 0;  
}
```

编译器生成了如下这样的函数：

![img](/img/in-post/2017-08-20-C++-generic-programming-basics/03.png)

其中，函数参数的类型是和调用函数传递的类型完全匹配的。

# 类型形参转换

一般不会转换实参以匹配已有的实例化,相反会产生新的实例。


举个栗子：对如下的函数调用：

```
template <typename T>  
T Add(T left, T right)  
{  
    return left + right;  
}  
  
int Add(int left, int right)  
{  
    return left + right;  
}  
  
int main()  
{  
    Add(1.2, 3.4);  
    return 0;  
}
```

即程序中已经实现过了Add函数的int版本，那么调用Add(1.2， 3.4)；时，编译器不会将1.2和3.4隐式转换为int型，进而调用已有的Add版本，而是重新合成一个double的版本：

![img](/img/in-post/2017-08-20-C++-generic-programming-basics/04.png)

当然前提是能够生成这么一个模板函数。如果这个模板函数无法生成的话，那么只能调用已有的版本了。

编译器只会执行两种转换:

1. const转换:接收const引用或者const指针的函数可以分别用非const对象的引用或者指针来调用
2. 数组或函数到指针的转换:如果模板形参不是引用类型,则对数组或函数类型的实参应用常规指针转换。数组实参将当做指向其第一个元素的指针,函数实参当做指向函数类型的指针。

第一种：

```
template <typename T>  
T Add(const T &left,const T &right)  
{  
    return left + right;  
}  
  
int main()  
{  
    Add(1.2, 3.4);  
    return 0;  
}
```

面对这样的传参，编译器是可以完成1.2到const double &类型的转换，因为这样做是安全的。

第二种：

```
template <typename T>  
T Add(const T &left,const T &right)  
{  
    return left + right;  
}  
  
int main()  
{  
    Add(1.2, 3.4);  
    return 0;  
}
```

完成了数组到指针的转换，因为数组在作为函数参数传递时，本身就会发生降级，形参接收到的是个指针，且指向该数组的第一个元素。

```
template <typename T>  
int sum(T *t)  
{  
    //do something  
}  
  
void fun()  
{  
    //do something  
}  
  
int main()  
{    
    sum(fun);  
    return 0;  
}
```

上例完成函数到函数指针的转换。

# 模板参数

函数模板有两种类型参数:模板参数和调用参数。模板参数又分为模板类型形参和非模板类型形参。

模板形参名字只能在模板形参之后到模板声明或定义的末尾之间使用,遵循名字屏蔽规则。

![img](/img/in-post/2017-08-20-C++-generic-programming-basics/05.png)

模板形参的名字在同一模板形参列表中只能使用一次。

![img](/img/in-post/2017-08-20-C++-generic-programming-basics/06.png)

所有模板形参前面必须加上class或者typename关键字修饰。

![img](/img/in-post/2017-08-20-C++-generic-programming-basics/07.png)

注意:在函数模板的内部不能指定缺省的模板实参。

模板类型形参是模板内部定义的常量,在需要常量表达式的时候,可以使用非模板类型参数。例如：

```
template <typename T, int size> //size即为非模板类型参数  
void sum(T (&arr)[size])  
{  
}  
  
int main()  
{  
    int arr[10];  
    sum(arr); //这里调用时，自动将10传递给size，作为数组元素个数  
    return 0;  
}
```

# 模板形参说明

1. 模板形参表使用括起来
2. 和函数参数表一样,跟多个参数时必须用逗号隔开,类型可以相同也可以不相同
3. 模板形参表不能为空
4. 模板形参可以是类型形参,也可以是非类型新参,类型形参跟在class和typename后
5. 模板类型形参可作为类型说明符用在模板中的任何地方,与内置类型或自定义类型使用方法完全相同,可用于指定函数形参类型、返回值、局部变量和强制类型转换
6. 模板形参表中,class和typename具有相同的含义,可以互换,使用typename更加直观。但关键字typename是作为C++标准加入到C++中的,旧的编译器可能不支持。

# 模板函数重载

```
int Max(const int& left, const int & right)  
{    return left>right? left:right;  
}  
template<typename T>  
T Max(const T& left, const T& right)  
{  
return left>right? left:right;  
}  
template<typename T>  
T Max(const T& a, const T& b, const T& c)  
{  
    return Max(Max(a, b), c);  
};  
int main()  
{  
    Max(10, 20, 30);  
    Max<>(10, 20); //3.用模板生成，而不是调用显示定义的同类型版本  
    Max(10, 20);  
    Max(10, 20.12);  
    Max<int>(10.0, 20.0); //显示告诉编译器T的类型  
    Max(10.0, 20.0);  
    return 0;  
}
```

# 说明

1. 一个非模板函数可以和一个同名的函数模板同时存在,而且该函数模板还可以被实例化为这个非模板函数
2. 对于非模板函数和同名函数模板,如果其他条件都相同,在调动时会优先调动非模板函数而不会从该模板产生出一个实例。如果模板可以产生一个具有更好匹配的函数,那么将选择模板
3. 显式指定一个空的模板实参列表,该语法告诉编译器只有模板才能来匹配这个调用,而且所有的模板参数都应该根据实参演绎出来
4. 模板函数不允许自动类型转换,但普通函数可以进行自动类型转换

上面的函数模板不能用来比较两个字符串，如果传递参数为字符串，返回的则是两个参数地址的比较，不是我们想要的结果。所以，又有了模板函数特化：

模板函数特化形式如下:

1. 关键字template后面接一对空的尖括号
2. 再接模板名和一对尖括号,尖括号中指定这个特化定义的模板形参
3. 函数形参表
4. 函数体

在模板特化版本的调用中,实参类型必须与特化版本函数的形参类型完全匹配,如果不匹配,编译器将为实参模板定义中实例化一个实例。

举一个栗子：

```
template <typename T>  
int cmp(const T &left, const T &right)  
{  
    return left - right;  
}  
  
template <>  
int cmp<const char * &>(const char * &p1, const char * &p2)  
{  
    return strcmp(p1, p2);  
}  
  
int main()  
{    
    const char *s1 = "abc";  
    const char *s2 = "abd";  
    cmp(s1, s2);  
    return 0;  
}
```

再次强调，实参类型必须与特化版本函数的形参类型完全匹配，哪怕实参前边修饰的没有const，而模板形参中有，也不会构成特化。特化版本是对应于此模板而写的，特化版本的参数类型必须完全与模板形参相同，如上述例子中，都为const &。

# 类模板

## 格式

```
template<class 形参名1, class 形参名2, …class 形参名n>
class 类名
{ … };
```

举个栗子：

```
// 以模板方式实现动态顺序表  
template<typename T>  
class SeqList  
{  
public :  
    SeqList();  
    ~ SeqList();  
private :  
    int _size ;  
    int _capacity ;  
    T* _data ;  
};  
template <typename T>  
SeqList <T>:: SeqList()  
    : _size(0)  
    , _capacity(10)  
    , _data(new T[ _capacity])  
{}  
template <typename T>  
SeqList <T>::~ SeqList()  
{  
    delete [] _data ;  
}  
void test1 ()  
{  
    SeqList<int > sl1;  
    SeqList<double > sl2;  
}
```

与调用函数模板形成对比,使用类模板时,必须为模板形参显式指定实参!

## 模板类的实例化

只要有一种不同的类型,编译器就会实例化出一个对应的类。


SeqList sl1;
SeqList sl2;


当定义上述两种类型的顺序表时,编译器会使用int和double分别代替模板形参,重新编写SeqList类,最后创建名为SeqList和SeqList的类。
