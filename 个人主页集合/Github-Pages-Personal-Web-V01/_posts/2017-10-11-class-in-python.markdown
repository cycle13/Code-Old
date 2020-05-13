---
layout:     post
title:      "Python 中的类"
subtitle:   "Class in Python"
date:       2017-10-11
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

在Python中，可以通过class关键字定义自己的类，然后通过自定义的类对象类创建实例对象。
例如，下面创建了一个Student的类，并且实现了这个类的初始化函数`__init__`:

```
class Student(object):
	count = 0
	books = []
	def __init__(self, name, age):
		self.name = name
		self.age = age
		pass
```

接下来就通过上面的Student类来看看Python中类的相关内容。

# 数据属性

在上面的Student类中，”count”"books”"name”和”age”都被称为类的数据属性，但是它们又分为类数据属性和实例数据属性。

## 类数据属性和实例数据属性

首先看一段代码，代码中分别展示了对类数据属性和实例数据属性的访问：

```
Student.books.extend(["python", "javascript"])
print "Student book list: %s" %Student.books
# class can add class attribute after class defination
Student.hobbies = ["reading", "jogging", "swimming"]
print "Student hobby list: %s" %Student.hobbies
print dir(Student)
print
wilber = Student("Wilber", 28)
print "%s is %d years old" %(wilber.name, wilber.age)
# class instance can add new attribute
# "gender" is the instance attribute only belongs to wilber
wilber.gender = "male"
print "%s is %s" %(wilber.name, wilber.gender)
# class instance can access class attribute
print dir(wilber)
wilber.books.append("C#")
print wilber.books
print
will = Student("Will", 27)
print "%s is %d years old" %(will.name, will.age)
# will shares the same class attribute with wilber
# will don't have the "gender" attribute that belongs to wilber
print dir(will)
print will.books
```

通过内建函数dir()，或者访问类的字典属性`__dict__`，这两种方式都可以查看类有哪些属性，代码的输出为：

![img](/img/in-post/2017-10-11-class-in-python/01.png)

对于类数据属性和实例数据属性，可以总结为：

1. 类数据属性属于类本身，可以通过类名进行访问/修改
2. 类数据属性也可以被类的所有实例访问/修改
3. 在类定义之后，可以通过类名动态添加类数据属性，新增的类属性也被类和所有实例共有
4. 实例数据属性只能通过实例访问
5. 在实例生成后，还可以动态添加实例数据属性，但是这些实例数据属性只属于该实例

## 特殊的类属性

对于所有的类，都有一组特殊的属性：

|类属性|含义|
|-----|---|
|`__name__`|类的名字（字符串）|
|`__doc__`|类的文档字符串|
|`__bases__`|类的所有父类组成的元组|
|`__dict__`|类的属性组成的字典|
|`__module__`|类所属的模块|
|`__class__`|类对象的类型|

通过这些属性，可以得到 Student类的一些信息：

```
class Student(object):
'''
this is a Student class
'''
count = 0
books = []
def __init__(self, name, age):
self.name = name
self.age = age
pass
print Student.__name__
print Student.__doc__
print Student.__bases__
print Student.__dict__
print Student.__module__
print Student.__class
```

代码输出为：

![img](/img/in-post/2017-10-11-class-in-python/02.png)

## 属性隐藏

从上面的介绍了解到，类数据属性属于类本身，被所有该类的实例共享；并且，通过实例可以去访问/修改类属性。但是，在通过实例中访问类属性的时候一定要谨慎，因为可能出现属性”隐藏”的情况。

继续使用上面的Student类，来看看属性隐藏：

```
wilber = Student("Wilber", 28)
print "Student.count is wilber.count: ", Student.count is wilber.count
wilber.count = 1
print "Student.count is wilber.count: ", Student.count is wilber.count
print Student.__dict__
print wilber.__dict__
del wilber.count
print "Student.count is wilber.count: ", Student.count is wilber.count
print
wilber.count += 3
print "Student.count is wilber.count: ", Student.count is wilber.count
print Student.__dict__
print wilber.__dict__
del wilber.count
print
print "Student.books is wilber.books: ", Student.books is wilber.books
wilber.books = ["C#", "Python"]
print "Student.books is wilber.books: ", Student.books is wilber.books
print Student.__dict__
print wilber.__dict__
del wilber.books
print "Student.books is wilber.books: ", Student.books is wilber.books
print
wilber.books.append("CSS")
print "Student.books is wilber.books: ", Student.books is wilber.books
print Student.__dict__
print wilber.__dict__
```

代码的输出为：

![img](/img/in-post/2017-10-11-class-in-python/03.png)

分析一下上面代码的输出：

* 对于不可变类型的类属性Student.count，可以通过实例wilber进行访问，并且”Student.count is wilber.count”
* 当通过实例赋值/修改count属性的时候，都将为实例wilber新建一个count实例属性，这时，”Student.count is not wilber.count”
* 当通过”del wilber.count”语句删除实例的count属性后，再次成为”Student.count is wilber.count”
* 同样对于可变类型的类属性Student.books，可以通过实例wilber进行访问，并且”Student. books is wilber. books”
* 当通过实例赋值books属性的时候，都将为实例wilber新建一个books实例属性，这时，”Student. Books is not wilber. books”
* 当通过”del wilber. books”语句删除实例的books属性后，再次成为”Student. books is wilber. books”
* 当通过实例修改books属性的时候，将修改wilber.books指向的内存地址（即Student.books），此时，”Student. Books is wilber. books”

注意，虽然通过实例可以访问类属性，但是，不建议这么做，最好还是通过类名来访问类属性，从而避免属性隐藏带来的不必要麻烦。


# 方法


在一个类中，可能出现三种方法，实例方法、静态方法和类方法，下面来看看三种方法的不同。

## 实例方法

实例方法的第一个参数必须是”self”，”self”类似于C++中的”this”。

实例方法只能通过类实例进行调用，这时候”self”就代表这个类实例本身。通过”self”可以直接访问实例的属性。

```
class Student(object):
'''
this is a Student class
'''
count = 0
books = []
def __init__(self, name, age):
self.name = name
self.age = age
def printInstanceInfo(self):
print "%s is %d years old" %(self.name, self.age)
pass
wilber = Student("Wilber", 28)
wilber.printInstanceInfo()
```

## 类方法

类方法以cls作为第一个参数，cls表示类本身，定义时使用@classmethod装饰器。通过cls可以访问类的相关属性。

```
class Student(object):
'''
this is a Student class
'''
count = 0
books = []
def __init__(self, name, age):
self.name = name
self.age = age
@classmethod
def printClassInfo(cls):
print cls.__name__
print dir(cls)
pass
Student.printClassInfo()
wilber = Student("Wilber", 28)
wilber.printClassInfo()
```

代码的输出为，从这段代码可以看到，类方法可以通过类名访问，也可以通过实例访问。

![img](/img/in-post/2017-10-11-class-in-python/04.png)

## 静态方法

与实例方法和类方法不同，静态方法没有参数限制，既不需要实例参数，也不需要类参数，定义的时候使用@staticmethod装饰器。
同类方法一样，静态法可以通过类名访问，也可以通过实例访问。

```
class Student(object):
'''
this is a Student class
'''
count = 0
books = []
def __init__(self, name, age):
self.name = name
self.age = age
@staticmethod
def printClassAttr():
print Student.count
print Student.books
pass
Student.printClassAttr()
wilber = Student("Wilber", 28)
wilber.printClassAttr()
```

这三种方法的主要区别在于参数，实例方法被绑定到一个实例，只能通过实例进行调用；但是对于静态方法和类方法，可以通过类名和实例两种方式进行调用。

## 访问控制

Python中没有访问控制的关键字，例如private、protected等等。但是，在Python编码中，有一些约定来进行访问控制。
单下划线”_”

在Python中，通过单下划线”_”来实现模块级别的私有化，一般约定以单下划线”_”开头的变量、函数为模块私有的，也就是说”from moduleName import *”将不会引入以单下划线”_”开头的变量、函数。

现在有一个模块lib.py，内容用如下，模块中一个变量名和一个函数名分别以”_”开头：

```
numA = 10
_numA = 100
def printNum():
print "numA is:", numA
print "_numA is:", _numA
def _printNum():
print "numA is:", numA
print "_numA is:", _numA
```

当通过下面代码引入lib.py这个模块后，所有的以”_”开头的变量和函数都没有被引入，如果访问将会抛出异常：

```
from lib import *
print numA
printNum()
print _numA

#print _printNum()
```

![img](/img/in-post/2017-10-11-class-in-python/05.png)

双下划线”__”

对于Python中的类属性，可以通过双下划线”__”来实现一定程度的私有化，因为双下划线开头的属性在运行时会被”混淆”（mangling）。

在Student类中，加入了一个”__address”属性：

```
class Student(object):
def __init__(self, name, age):
self.name = name
self.age = age
self.__address = "Shanghai"
pass
wilber = Student("Wilber", 28)
print wilber.__address
```

当通过实例wilber访问这个属性的时候，就会得到一个异常，提示属性”__address”不存在。

![img](/img/in-post/2017-10-11-class-in-python/06.png)

其实，通过内建函数dir()就可以看到其中的一些原由，”__address”属性在运行时，属性名被改为了”_Student__address”（属性名前增加了单下划线和类名）

```
>>> wilber = Student("Wilber", 28)
>>> dir(wilber)
['_Student__address', '__class__', '__delattr__', '__dict__', '__doc__', '__form
at__', '__getattribute__', '__hash__', '__init__', '__module__', '__new__', '__r
educe__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__', '
__subclasshook__', '__weakref__', 'age', 'name']
>>>
```

所以说，即使是双下划线，也没有实现属性的私有化，因为通过下面的方式还是可以直接访问”__address”属性：

```
>>> wilber = Student("Wilber", 28)
>>> print wilber._Student__address
Shanghai
>>>
```

双下划线的另一个重要的目地是，避免子类对父类同名属性的冲突。

看下面一个例子：

```
class A(object):
def __init__(self):
self.__private()
self.public()
def __private(self):
print 'A.__private()'
def public(self):
print 'A.public()'
class B(A):
def __private(self):
print 'B.__private()'
def public(self):
print 'B.public()'
b = B()
```

当实例化B的时候，由于没有定义`__init__`函数，将调用父类的`__init__`，但是由于双下划线的”混淆”效果，`self.__private()`将变成 `self._A__private()`。


看到这里，就清楚为什么会有如下输出了：

![img](/img/in-post/2017-10-11-class-in-python/07.png)

“_”和” __”的使用 更多的是一种规范/约定，不没有真正达到限制的目的：

“_”：以单下划线开头的表示的是protected类型的变量，即只能允许其本身与子类进行访问；同时表示弱内部变量标示，如，当使用”from moduleNmae import *”时，不会将以一个下划线开头的对象引入。

“__”：双下划线的表示的是私有类型的变量。只能是允许这个类本身进行访问了，连子类也不可以，这类属性在运行时属性名会加上单下划线和类名。

# 继承

在Python中，同时支持单继承与多继承，一般语法如下：

```
class SubClassName(ParentClass1 [, ParentClass2, ...]):
class_suite
```

实现继承之后，子类将继承父类的属性，也可以使用内建函数insubclass()来判断一个类是不是另一个类的子孙类：

```
class Parent(object):
'''
parent class
'''
numList = []
def numAdd(self, a, b):
return a+b
class Child(Parent):
pass
c = Child()
# subclass will inherit attributes from parent class
Child.numList.extend(range(10))
print Child.numList
print "2 + 5 =", c.numAdd(2, 5)
# built-in function issubclass()
print issubclass(Child, Parent)
print issubclass(Child, object)
# __bases__ can show all the parent classes
print Child.__bases__
# doc string will not be inherited
print Parent.__doc__
print Child.__doc__
```

代码的输出为，例子中唯一特别的地方是文档字符串。文档字符串对于类，函数/方法，以及模块来说是唯一的，也就是说`__doc__`属性是不能从父类中继承来的。

## 继承中的`__init__`

当在Python中出现继承的情况时，一定要注意初始化函数`__init__`的行为。

1.如果子类没有定义自己的初始化函数，父类的初始化函数会被默认调用；但是如果要实例化子类的对象，则只能传入父类的初始化函数对应的参数，否则会出错。

```
class Parent(object):
def __init__(self, data):
self.data = data
print "create an instance of:", self.__class__.__name__
print "data attribute is:", self.data

class Child(Parent):
pass

c = Child("init Child")
print
c = Child()
```

代码的输出为：

![img](/img/in-post/2017-10-11-class-in-python/08.png)

2.如果子类定义了自己的初始化函数，而没有显示调用父类的初始化函数，则父类的属性不会被初始化

```
class Parent(object):
def __init__(self, data):
self.data = data
print "create an instance of:", self.__class__.__name__
print "data attribute is:", self.data
class Child(Parent):
def __init__(self):
print "call __init__ from Child class"
c = Child()
print c.data
```

代码的输出为：

![img](/img/in-post/2017-10-11-class-in-python/09.png)

3.如果子类定义了自己的初始化函数，显示调用父类，子类和父类的属性都会被初始化

```
class Parent(object):
def __init__(self, data):
self.data = data
print "create an instance of:", self.__class__.__name__
print "data attribute is:", self.data
class Child(Parent):
def __init__(self):
print "call __init__ from Child class"
super(Child, self).__init__("data from Child")
c = Child()
print c.data
```

代码的输出为：

![img](/img/in-post/2017-10-11-class-in-python/10.png)

## super

前面一个例子中，已经看到了通过super来调用父类`__init__`方法的例子，下面看看super的使用。

在子类中，一般会定义与父类相同的属性（数据属性，方法），从而来实现子类特有的行为。也就是说，子类会继承父类的所有的属性和方法，子类也可以覆盖父类同名的属性和方法。

```
class Parent(object):
fooValue = "Hi, Parent foo value"
def foo(self):
print "This is foo from Parent"
class Child(Parent):
fooValue = "Hi, Child foo value"
def foo(self):
print "This is foo from Child"
c = Child()
c.foo()
print Child.fooValue
```

在这段代码中，子类的属性”fooValue”和”foo”覆盖了父类的属性，所以子类有了自己的行为。

![img](/img/in-post/2017-10-11-class-in-python/11.png)

但是，有时候可能需要在子类中访问父类的一些属性：

```
class Parent(object):
fooValue = "Hi, Parent foo value"
def foo(self):
print "This is foo from Parent"
class Child(Parent):
fooValue = "Hi, Child foo value"
def foo(self):
print "This is foo from Child"
print Parent.fooValue
# use Parent class name and self as an argument
Parent.foo(self)
c = Child()
c.foo()
```

这时候，可以通过父类名直接访问父类的属性，当调用父类的方法是，需要将”self”显示的传递进去的方式。

![img](/img/in-post/2017-10-11-class-in-python/12.png)

这种方式有一个不好的地方就是，需要经父类名硬编码到子类中，为了解决这个问题，可以使用Python中的super关键字：

```
class Parent(object):
fooValue = "Hi, Parent foo value"
def foo(self):
print "This is foo from Parent"

class Child(Parent):
fooValue = "Hi, Child foo value"
def foo(self):
print "This is foo from Child"
# use super to access Parent attribute
print super(Child, self).fooValue
super(Child, self).foo()

c = Child()
c.foo()
```

对于”super(Child, self).foo()”可以理解为，首先找到Child的父类Parent，然后调用父类的foo方法，同时将Child的实例self传递给foo方法。

但是，如果当一个子类有多个父类的时候，super会如何工作呢？这是就需要看看MRO的概念了。

## MRO

假设现在有一个如下的继承结构，首先通过类名显示调用的方式来调用父类的初始化函数：

```
class A(object):
def __init__(self):
print " ->Enter A"
print " <-Leave A"
class B(A):
def __init__(self):
print " -->Enter B"
A.__init__(self)
print " <--Leave B"
class C(A):
def __init__(self):
print " --->Enter C"
A.__init__(self)
print " <---Leave C"
class D(B, C):
def __init__(self):
print "---->Enter D"
B.__init__(self)
C.__init__(self)
print "<----Leave D"
d = D()
```

从输出中可以看到，类A的初始化函数被调用了两次，这不是我们想要的结果：

![img](/img/in-post/2017-10-11-class-in-python/13.png)

下面，我们通过super方式来调用父类的初始化函数：

```
class A(object):
def __init__(self):
print " ->Enter A"
print " <-Leave A"
class B(A):
def __init__(self):
print " -->Enter B"
super(B, self).__init__()
print " <--Leave B"
class C(A):
def __init__(self):
print " --->Enter C"
super(C, self).__init__()
print " <---Leave C"
class D(B, C):
def __init__(self):
print "---->Enter D"
super(D, self).__init__()
print "<----Leave D"
d = D()
```

通过输出可以看到，当使用super后，A的初始化函数只能调用了一次：

![img](/img/in-post/2017-10-11-class-in-python/14.png)

为什么super会有这种效果？下面就开始看看Python中的方法解析顺序MRO（Method Resolution Order）。

Python的类有一个`__mro__`属性，这个属性中就保存着方法解析顺序。结合上面的例子来看看类D的`__mro__`:

```
>>> print "MRO:", [x.__name__ for x in D.__mro__]
MRO: ['D', 'B', 'C', 'A', 'object']
>>>
```

看到这里，对于上面使用super例子的输出就应该比较清楚了。

* Python的多继承类是通过MRO的方式来保证各个父类的函数被逐一调用，而且保证每个父类函数只调用一次（如果每个类都使用super）
* 混用super类和非绑定的函数是一个危险行为，这可能导致应该调用的父类函数没有调用或者一个父类函数被调用多次

## `__slots__`

从前面的介绍可以看到，当我们通过一个类创建了实例之后，仍然可以给实例添加属性，但是这些属性只属于这个实例。

有些时候，我们可以需要限制类实例对象的属性，这时就要用到类中的`__slots__`属性了。`__slots__`属性对于一个tuple，只有这个tuple中出现的属性可以被类实例使用。

```
class Student(object):
__slots__ = ("name", "age")
def __init__(self, name, age):
self.name = name
self.age = age
s = Student("Wilber", 28)
print "%s is %d years old" %(s.name, s.age)
s.score = 96
```

在这个例子中，当场是给Student的实例s添加一个score属性的时候，就会遇到下面的异常：

![img](/img/in-post/2017-10-11-class-in-python/15.png)

## 子类没有`__slots__`属性

使用`__slots__`要注意，`__slots__`定义的属性仅对当前类的实例起作用，对继承的子类实例是不起作用的：

```
class Person(object):
__slots__ = ("name", "age")
pass
class Student(Person):
pass
s = Student()
s.name, s.age = "Wilber", 28
s.score = 100
print "%s is %d years old, score is %d" %(s.name, s.age, s.score)
```

从代码的输出可以看到，子类Student的实例并不受父类中__slots__属性的限制：

![img](/img/in-post/2017-10-11-class-in-python/16.png)

## 子类拥有`__slots__`属性

但是，如果子类本身也有`__slots__`属性，子类的属性就是自身的`__slots__`加上父类的`__slots__`。

```
class Person(object):
__slots__ = ("name", "age")
pass
class Student(Person):
__slots__ = ("score", )
pass
s = Student()
s.name, s.age = "Wilber", 28
s.score = 100
print "%s is %d years old, score is %d" %(s.name, s.age, s.score)
print s.__slots__
s.city = "Shanghai"
```

代码的输出为：

![img](/img/in-post/2017-10-11-class-in-python/17.png)

所以说，对于`__slots__`属性：

* 如果父类包含对`__slots__`的定义，子类不包含对`__slots__`的定义，解释器忽略`__slots__`的作用
* 如果父类包含对`__slots__`的定义，子类包含对`__slots__`的定义，并且无论元组的的元素个数，解释器都会按照父类的`__slots__`和子类的`__slots__`的并集来检查

# 类构造和初始化

在前面的文章中，经常使用初始化函数`__init__`，下面看看`__init__`和`__new__`的联系和差别。

下面先通过一段代码看看这两个方法的调用顺序：

```
class A(object):
def __init__(self,*args, **kwargs):
print "init %s" %self.__class__
def __new__(cls,*args, **kwargs):
print "new %s" %cls
return object.__new__(cls, *args, **kwargs)
a = A()
```

从代码的输出可以看到，当通过类实例化一个对象的时候，`__new__`方法首先被调用，然后是`__init__`方法。

![img](/img/in-post/2017-10-11-class-in-python/18.png)

一般来说，`__init__`和`__new__`函数都会有下面的形式：

```
def __init__(self, *args, **kwargs):
# func_suite
def __new__(cls, *args, **kwargs):
# func_suite
return obj
```

对于`__new__`和`__init__`可以概括为：

* `__new__`方法在Python中是真正的构造方法（创建并返回实例），通过这个方法可以产生一个”cls”对应的实例对象，所以说`__new__`方法一定要有返回
* 对于`__init__`方法，是一个初始化的方法，”self”代表由类产生出来的实例对象，`__init__`将对这个对象进行相应的初始化操作

前面文章中已经介绍过了`__init__`的一些行为，包括继承情况中`__init__`的表现。下面就重点看看`__new__`方法。

## `__new__`特性

`__new__`是在新式类中新出现的方法，它有以下行为特性：

* `__new__` 方法是在类实例化对象时第一个调用的方法，将返回实例对象
* `__new__` 方法始终都是类的静态方法（即第一个参数为cls），即使没有被加上静态方法装饰器
* 第一个参数cls是当前正在实例化的类，如果要得到当前类的实例，应当在当前类中的 `__new__` 方法语句中调用当前类的父类的 `__new__` 方法

对于上面的第三点，如果当前类是直接继承自 object，那当前类的 `__new__` 方法返回的对象应该为：

```
def __new__(cls, *args, **kwargs):
# func_suite
return object.__new__(cls, *args, **kwargs)
```

## 重写`__new__`

如果（新式）类中没有重写`__new__`方法，Python默认是调用该类的直接父类的`__new__`方法来构造该类的实例，如果该类的父类也没有重写`__new__`，那么将一直按照同样的规则追溯至object的`__new__`方法，因为object是所有新式类的基类。

而如果新式类中重写了`__new__`方法，那么可以选择任意一个其他的新式类（必须是新式类，只有新式类有`__new__`，因为所有新式类都是从object派生）的`__new__`方法来创建实例，包括这个新式类的所有前代类和后代类，只要它们不会造成递归死循环。

看一段例子代码：

```
class Foo(object):
def __new__(cls, *args, **kwargs):
obj = object.__new__(cls, *args, **kwargs)
# 这里的object.__new__(cls, *args, **kwargs) 等价于
# super(Foo, cls).__new__(cls, *args, **kwargs)
# object.__new__(Foo, *args, **kwargs)
# Bar.__new__(cls, *args, **kwargs)
# Student.__new__(cls, *args, **kwargs)，即使Student跟Foo没有关系，也是允许的，因为Student是从object派生的新式类
# 在任何新式类，不能调用自身的“__new__”来创建实例，因为这会造成死循环
# 所以要避免return Foo.__new__(cls, *args, **kwargs)或return cls.__new__(cls, *args, **kwargs)
print "Call __new__ for %s" %obj.__class__
return obj
class Bar(Foo):
def __new__(cls, *args, **kwargs):
obj = object.__new__(cls, *args, **kwargs)
print "Call __new__ for %s" %obj.__class__
return obj
class Student(object):
# Student没有“__new__”方法，那么会自动调用其父类的“__new__”方法来创建实例，即会自动调用 object.__new__(cls)
pass
class Car(object):
def __new__(cls, *args, **kwargs):
# 可以选择用Bar来创建实例
obj = object.__new__(Bar, *args, **kwargs)
print "Call __new__ for %s" %obj.__class__
return obj
foo = Foo()
bar = Bar()
car = Car()
```

代码的输出为：

![img](/img/in-post/2017-10-11-class-in-python/19.png)

## `__init__`的调用

`__new__`决定是否要使用该类的`__init__`方法，因为`__new__` 可以调用其他类的构造方法或者直接返回别的类创建的对象来作为本类的实例。

通常来说，新式类开始实例化时，`__new__`方法会返回cls（cls指代当前类）的实例，然后调用该类的`__init__`方法作为初始化方法，该方法接收这个实例（即self）作为自己的第一个参数，然后依次传入`__new__`方法中接收的位置参数和命名参数。

但是，如果`__new__`没有返回cls（即当前类）的实例，那么当前类的`__init__`方法是不会被调用的。看下面的例子：

```
class A(object):
def __init__(self, *args, **kwargs):
print "Call __init__ from %s" %self.__class__
def __new__(cls, *args, **kwargs):
obj = object.__new__(cls, *args, **kwargs)
print "Call __new__ for %s" %obj.__class__
return obj
class B(object):
def __init__(self, *args, **kwargs):
print "Call __init__ from %s" %self.__class__
def __new__(cls, *args, **kwargs):
obj = object.__new__(A, *args, **kwargs)
print "Call __new__ for %s" %obj.__class__
return obj
b = B()
print type(b)
```

代码中，在B的`__new__`方法中，通过`obj = object.__new__(A, *args, **kwargs)`创建了一个A的实例，在这种情况下，B的`__init__`函数就不会被调用到。

## 派生不可变类型

关于`__new__`方法还有一个重要的用途就是用来派生不可变类型。

例如，Python中float是不可变类型，如果想要从float中派生一个子类，就要实现`__new__`方法：

```
class Round2Float(float):
def __new__(cls, num):
num = round(num, 2)
#return super(Round2Float, cls).__new__(cls, num)
return float.__new__(Round2Float, num)
f = Round2Float(4.14159)
print f
```

代码中从float派生出了一个Round2Float类，该类的实例就是保留小数点后两位的浮点数。

## 定制一个类

在Python中，我们可以通过”魔术方法”使自定义的class变得强大、易用。

例如，前面的文章中介绍过Python迭代器，当我们想定义一个可迭代的类对象的时候，就可以去实现`__iter__(self)`这个魔术方法；
又例如，前面文章介绍的上下文管理器，当需要建立一个上下文管理器类对象的时候，就可以去实现`__enter__(self)`和`__exit__(self)`方法。

所以，建议参考 “魔术方法” 的文档，通过魔术方法来定制自定义的类。

## 调用魔术方法

一些魔术方法直接和内建函数相对应的，在这种情况下，调用他们的方法很简单，下面给出了一些对应表。

| 魔术方法 | 调用方式 | 解释|
|---------|--------|-----|
|`__new__(cls [,...])`| `instance = MyClass(arg1, arg2)`| `__new__` 在创建实例的时候被调用|
| `__init__(self [,...])` | `instance = MyClass(arg1, arg2)` | `__init__` 在创建实例的时候被调用|
| `__cmp__(self, other)` | `self == other, self > other`, 等。| 在比较的时候调用|
|`__pos__(self)`|`+self`|一元加运算符|
|`__neg__(self)`|`-self`|一元减运算符|
|`__invert__(self)`|`~self`|取反运算符|
|`__index__(self)`|`x[self]`|对象被作为索引使用的时候|
|`__nonzero__(self)`|`bool(self)`|对象的布尔值|
|`__getattr__(self, name)`|`self.name # name `不存在|访问一个不存在的属性时|
|`__setattr__(self, name, val)`|`self.name = val`|对一个属性赋值时|
|`__delattr__(self, name)`|`del self.name`|删除一个属性时|
|`__getattribute(self, name)`|`self.name`|访问任何属性时|
|`__getitem__(self, key)`|`self[key]`|使用索引访问元素时|
|`__setitem__(self, key, val)`|`self[key] = val`|对某个索引值赋值时|
|`__delitem__(self, key)`|`del self[key]`|删除某个索引值时|
|`__iter__(self)`|`for x in self`|迭代时|
|`__contains__(self, value)`|`value in self, value not in self`|使用 in 操作测试关系时|
|`__concat__(self, value)`|`self + other`|连接两个对象时|
|`__call__(self [,...])`|`self(args)`|“调用”对象时|
|`__enter__(self)`|`with self as x:`|with 语句环境管理|
|`__exit__(self, exc, val, trace)`|`with self as x:`|with 语句环境管理|
|`__getstate__(self)`|`pickle.dump(pkl_file, self)`|序列化|
|`__setstate__(self)`|`data = pickle.load(pkl_file)`|序列化|


