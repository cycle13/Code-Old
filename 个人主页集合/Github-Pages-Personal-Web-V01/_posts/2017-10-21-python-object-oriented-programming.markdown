---
layout:     post
title:      "Python中的面向对象编程"
subtitle:   "Python object oriented programming"
date:       2017-10-21
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

# 什么是OOP（面向对象编程）？

所谓OOP，就是在编程中通过创建“对象”来模拟现实中需要解决的问题中的实体的一种编程模式。“对象”定义了两部分内容：数据，一般称为“域”（field）或者“属性”（attribute），以及对数据的操作，一般称为方法（method）。在编程术语中，把对对象的类型定义称为“类”（class）。对象即是类的一个实例。

# 说到OOP就不得不提OOP的三个基本特征：

## 封装

就是把数据和操作数据的方法都定义在一个类的内部，这样做的目的是保证一个对象的内部结构只能被它暴露给外面的方法来改变，而别的对象无法改变其他对象的内部状态。

## 继承

通常我们在解决问题过程中建立的模型会形成很庞大的层次结构，而这些模型（类）之间有相互关系。在这个类的层次体系中的从属(is a)关系在OOP中使用继承来实现，也就是说在某个对象中存在的特性在它派生出的对象中也会存在。 e.g. 假设你想架设自己的网上商店卖一些电子设备（Gadget）。这些Gadget有这些属性：电池容量，重量，内存容量，操作系统等。如果你想卖一个平板电脑（Tablet），你可以定义：平板电脑（Tablet）是一个“电子设备”（Gadget），在实现的代码中可以使用继承把Gadget类的所有属性和功能传递给平板电脑（Tablet）类。

## 多态

 即某个对象由于其子类的存在而表现出某些与其自身定义方法不一样的特性。多态是与继承相关的特性。 举例来说，假如你想在你的网上商店里卖智能手表（Smart Watch).智能手表是属于Gadget类,所有的Gadget都可以重启(restart),因此重启可以作为Gadget类的方法。由于Tablet和Smart Watch都继承了Gadget类，因此都继承了restart这个方法，但是Smart Watch的restart操作和Tablet的restart操作并不相同.所有如果我们把Smart Watch和Tablet都重启一下，我们使用同样的restart方法，但是每个对象执行这个操作的时候调用自己的方法。也就是说，从更抽象的层次看，当对两个不同Gadget执行同样的restart操作时，由于各自类型的不同它们的行为是不同的。

# Python是什么？ 

Python是动态类型的通用的高级编程语言，由Guido van Rossum在1991年设计开发并公布于众。

Python在当下是非常流行的编程语言，而由于有众多web框架如django，flask，bottlepy, pyramid等的存在，Python在web开发中得到广泛应用。

Python语言得名于Guido非常喜爱的Monty Python’s Flying Circus.

## 2.x和3.x， 我该用哪个版本？

Python 2.7.x版本官方支持到2020年。Python核心开发团队承诺不会有Python2.8，而对2.x版本将只提供安全更新和bug fix. 3.x分支早在2000年就发布，起初问题不断，但目前已工作良好。由于不提供向后兼容性，而大量的应用使用2.x版本实现，为了不破坏现有的应用，主流的linux发行版依旧默认提供2.x版本，因此目前转换到Python 3.x版本有些困难。Python 3.x版本可以在2.x版本存在的情况下便捷的安装并且使用而不会带来问题。 

关于2.x与3.x版本区别的更多详细解释，优缺点等请参考： 
Python.org – Should I use Python 2 or Python 3 for my development activity?
The Treehouse blog – Python 2 VS Python 3

# Python中的类

如上文所述，类是对象的类型定义。Python中对类的定义使用class关键字完成。

在Python编程中有一个惯例，通常一个.py文件中只包含一个类定义。虽然可以不这么做但是建议遵守这条规则。下面是对于上文举例的Gadget类的定义：

```
class Gadget:
weight = 100
operating_system = None
battery_capacity = 2000
screen_size = 1
my_iphone = Gadget()
```

Gadget类有四个属性：weight, operating_system, battery_capacity和screen_size. 我们可以用类的名称加括号来创建新的实例：Gadget()。但是看一下Gadget类各个属性的默认值，考虑到iPhone的实际情况，默认的属性值可能并不适用。我们需要一个方法，可以指定Gadget类的实例各个属性的值。这个用于帮助创建类实例的方法称为构造器(constructor).下面是一个Python中实现构造器(`__init__`)的例子。`__init__`在实例被创建后紧接着被调用，第一个参数为self，这是Python的惯例，代表刚刚被创建的对象。下面的例子展示了如何在构造器内部设置相关的属性：

```
class Gadget:
weight = 100
operating_system = None
battery_capacity = 2000
screen_size = 1

def __init__(self, weight, operating_system, battery_capacity, screen_size):
self.weight = weight
self.operating_system = operating_system
self.battery_capacity = battery_capacity
self.screen_size = screen_size
```

如果想知道给新对象设置了什么属性值，可以打印出来观察：

```
my_iphone = Gadget(weight = 128, operating_system="iOS", battery_capacity=2800, screen_size=4)

print(my_iphone.weight)
print(my_iphone.operating_system)
print(my_iphone.battery_capacity)
print(my_iphone.screen_size) 
```

但是这个解决方案有个问题，我们破坏了封装的特性：即可以直接访问my_iphone这个对象的内部状态，并且可以直接给screen_size或者operating_system等属性直接赋值。我们可以使用property关键字来避免这种直接操作属性的情形，使当前类成员对外部不可见： 

译注：由于在英文中attribute和property翻译过来都是属性，为避免混淆，下文中property将不再翻译

```
class Gadget:
"""A class used for modelling Gadgets in a web shop."""
__weight = 100
__operating_system = None
__battery_capacity = 2000
__screen_size = 1

def __init__(self, weight, operating_system, battery_capacity, screen_size):
self.__weight = weight
self.__operating_system = operating_system
self.__battery_capacity = battery_capacity
self.__screen_size = screen_size

def get_weight(self):
return self.__weight

def set_weight(self, weight):
self.__weight = weight

weight = property(get_weight, set_weight)

@property
def operating_system(self):
return self.__operating_system

@operating_system.setter
def operating_system(selg, new_os):
self.__operating_system = new_os 
```

我们可以通过用双下划线开头（`__attribute_name`)的形式来定义私有属性（更多详细信息参考….)，同时我们可以定义setter和getter来访问和操作这些属性。getter和setter方法可以满足封装的需要，但是同时带来Java式的编程体验：类的属性变量可以通过get和set方法设置。

而对开发者更友好的方式是使用property。在上面的例子中我分别用两种方法创建了两个property：weight和operating_system。 

在创建weight property的时候，我用weight = property(get_weight, set_weight）.在Python类定义中如果事先已经实现了Java式的get/set方法，使用property(get..., set…)这种语法可以很便捷的给类增加property。

而在实现operating_systm property时，我用了另外一称为注释/装饰器的方法。
首先定义了get方法，但是省略了’get’关键字（注意operating_system这个使用@property装饰的方法只有一个参数self）；然后创建了setter，同样也是命名为operating_system但有两个参数： self和 new_os, new_os是将要给operating_system设置的值。 下面是例子:

```
>>> from Gadget import Gadget
>>> my_iphone = Gadget(240,'iOS',1980,4)
>>> my_iphone.weight
240
>>> my_iphone.weight = 255
>>> my_iphone.weight
255
>>>
>>>
>>> my_iphone.operating_system
'iOS'
>>> my_iphone.operating_system = 'iOS 8.1'
>>> my_iphone.operating_system
'iOS 8.1'
>>>
```

使用property非常简单，参考上面的例子。基本上property与公有的成员变量一样使用，但是通过getter和setter我们可以对将要设置的值进行验证.(例子中没有做任何验证，但事实上是可以的） Python面向对象编程-第二部分很快发出来。

 在Part1中我介绍了面向对象编程的三大支柱：封装、继承和多态。并且已经讲解了Python中封装的相关知识，接下来我们谈谈继承。

## 什么是继承？

继承作为面向对象编程中的一个概念，它可以帮助程序员实现：

1. 建立模型 - 一种关系树（不是所有的编程语言都这样）
2. 重用代码 - 帮助开发者遵守DRY原则和重用现有的实现和逻辑代码
3. 扩展功能 - 有些时候不能改变现行类的源代码（别人都在用它，或者它不是public或干脆它就是不允许修改）; 要延伸的类的功能，就可以通过继承来实现。

Python支持单一和多重继承。在Python 2.x和3.x中，继承的原理和实现存在很大的差异。我给出的的例子都是基于Python 3.x的。

## Python中的单一继承

在Python中，继承可以通过这的语法实现： class MySubClass(MyBaseClass)
先声明新的子类，然后括号中指明继承的基类（或称为父类、超类）。
举个常用的例子： Animal作为一个基类，Panda, Pig等各种动物就可以作为它的子类。

来看一段代码：

```
class Animal:
    __name = None
    __age = 0
    __is_hungry = False
    __nr_of_legs = 0

    def __init__(self, name, age, is_hungry, nr_of_legs):
        self.name = name
        self.age = age
        self.is_hungry = is_hungry
        self.nr_of_legs = nr_of_legs

    #
    # METHODS
    #
    def eat(self, food):
        print("{} is eating {}.".format(self.name, food))    

    #
    # PROPERTIES
    #    
    @property
    def name(self):
        return self.__name

    @name.setter    
    def name(self,new_name):
        self.__name = new_name

    @property    
    def age(self):        
        return self.__age

    @age.setter    
    def age(self,new_age):
        self.__age = new_age

    @property    
    def is_hungry(self):        
        return self.__is_hungry

    @is_hungry.setter    
    def is_hungry(self,new_is_hungry):
        self.__is_hungry = new_is_hungry

    @property    
    def nr_of_legs(self):        
        return self.__nr_of_legs

    @nr_of_legs.setter    
    def nr_of_legs(self,new_nr_of_legs):
        self.__nr_of_legs = new_nr_of_legs
```

在Animal类中，定义了四个私有成员(`__name`, `__age`, `__is_hungry`, `__nr_of_legs`)。然后分别使用@property定义每个成员的属性（这个在Part1的内容中有提到）。然后我创建了一个构造函数，带有4个参数（不包含self），使用@property设定私有成员的值。基于这4个property，我创建了一个方法叫eat(self, food)，它会打印出 X is eating Y，其中X 是动物，Y是入参。接下来，我们把Animal作为一个基类来使用，用继承创建一条Snake。

```
class Snake(Animal):
    __temperature = 28    

    def __init__(self, name, temperature):
        super().__init__(name, 2, True, 0)
        self.temperature = temperature

    #
    # METHODS
    #
    def eat(self, food):
        if food == "meat":
            super().eat(food)        
        else:
            raise ValueError    

    #
    # PROPERTIES
    #
    @property    
    def temperature(self):        
        return self.__temperature

    @temperature.setter    
    def temperature(self,new_temperature):        
        if new_temperature < 10 or new_temperature > 40:
            raise ValueError
        self.__temperature = new_temperature
```

在Snake类中存在两个参数：name和temperature，对于`__temperature`这个私有成员，我创建一个property来接受入参。在构造函数里，我首先使用super()调用基类的构造函数（还有其他的方法来调用，但在Python 3.x中，这是推荐的方式）。当调用基类的构造函数时，我会传入默认值，例如`nr_of_legs`默认值zero，因为蛇没有腿，is_hungry默认值为True，因为蛇通常比其他动物要饿.

跟其他编程语言一样，在Python中也可以重写方法。我已经重写了eat方法，并且我加了额外的逻辑：如果给出的食物不是肉，那么我会抛出一个ValueError；如果是，我就会调用基类Animal中定义的eat方法。

## Python中的多重继承

Python为我们提供了从多个基类继承的可能性，叫做多重继承。如果有一些不同功能的类，而且这些不同的功能需要被组合在一起使用，那么多重继承将会非常有用。

在Python中，多重继承的语法非常简单，只要在新子类后的括号内填写所有的基类即可，例如：class MySubClass(MyBaseClass1, MyBaseClass2, MyBaseClass3)。

```
class Phone:    
    def __init__(self):
        print("Phone constructor invoked.")    
    def call_number(self, phone_number):
        print("Calling number {}".format(phone_number))

class Computer:
    def __init__(self):
        print("Computer constructor invoked.")
    def install_software(self, software):
        print("Computer installing the {} software".format(software))

class SmartPhone(Phone,Computer):
    def __init__(self):
        super().__init__()
```

我定义了3个类：Phone, Computer, SmartPhone
其中2个是基类：Phone, Computer；1个是继承类：SmartPhone
从逻辑上讲，这样做是正确的：一部智能手机可以打电话也可以安装软件，SmartPhone 应该具有call_number方法（从Phone继承）和install_software方法（从Computer继承）

```
#
# will be discussed later why only the constructor of Phone class was invoked
#
>>> my_iphone = SmartPhone()
Phone constructor invoked.

>>> my_iphone.call_number("123456789")
Calling number 123456789
>>> my_iphone.install_software("python")
Computer installing the python software
```

我们来看SmartPhone这个类的构造函数，挺简单，它包含了基类的构造函数，看上去没错，它通过继承获得的。但问题是从哪个基类继承的。如果仔细代码，会发现它仅仅从Phone继承的构造函数，这是为什么？
这个问题不是很好解释，它与Python的MRO（Method Resolution Order）有关系。

# Python中的MRO

MRO是一套规则，用于定义和确定类的线性化（linearization）。线性化（也称为优先级列表）是一个由近及远的基类的列表。MRO对于支持多重继承的编程语言十分重要，它有助于编程语言处理Diamond Problem。

在Python2.3，为了更好的定义多重继承时具体的MRO，发生了一些根本性的变化（这个版本采用C3线性化）。Michele Simionato 写过一篇很棒的帖子，说明了相关的方法和算法，帖子相当长，包含了大量的例子和算法的详细说明。Guido van Rossum也写过一篇关于Python MRO的详细文章。Perl语言同样是使用C3线性化算法来定义MRO的。

为了给回答我前面提出的问题：为什么`super().__init__()`只调用Phone的构造函数？出现这种情况，是因为基类的解析顺序。当`super().__init__()`调用Phone（MRO中的第一个基类），其他基类的`__init__()`没有被调用。MRO影响基类的结构函数如何被调用，我作为一个程序猿，当我使用多重继承时，必须确保我的基类全部被正确的初始化。我更新SmartPhone的代码以确保Computer和Phone都被初始化。（如果使用Python2.x，请确保你获得了相同的MRO）

```
class Phone(object):    
    def __init__(self):
        print("Phone constructor invoked.")    
    def call_number(self, phone_number):
        print("Calling number {}".format(phone_number))

class Computer(object):
    def __init__(self):
        print("Computer constructor invoked.")
    def install_software(self, software):
        print("Computer installing the {} software".format(software))

class SmartPhone(Phone, Computer):
    def __init__(self):
        Phone.__init__(self)        
        Computer.__init__(self)
```

现在创建了一个新的SmartPhone，这两个基类的构造函数都被调用了。

```
Python 3.2.3 (default, Feb 27 2014, 21:31:18)
[GCC 4.6.3] on linux2
Type "help", "copyright", "credits" or "license" for more information.
>>> from oop_multi import SmartPhone
>>> s = SmartPhone()
Phone constructor invoked.
Computer constructor invoked.
>>>
```

在Python中，对一个类使用 `mro()` 或者`__mro__`能够显示出类的MRO。
例如：

```
>>> SmartPhone.mro()
[<class 'SmartPhone'>, <class 'Phone'>, <class 'Computer'>, <class 'object'>]
>>> SmartPhone.__mro__
(<class 'SmartPhone'>, <class 'Phone'>, <class 'Computer'>, <class 'object'>)
```

MRO中的第一个元素，是我们使用mro()这个命令的类SmartPhone，接着是Phone，最后是Computer，这三个基类是如此设定的。

在一些拥有复杂层次结构的大型类中定义MRO将会十分困难，甚至在一些特定情况下MRO是无法定义的。Python将会抛出TypeError 以防基类间出现交叉引用，例如：

```
class Phone(object):
    def __init__(self):
        print("Phone constructor invoked.")    
    def call_number(self, phone_number):
        print("Calling number {}".format(phone_number))

class Computer(object):
    def __init__(self):
        print("Computer constructor invoked.")
    def install_software(self, software):
        print("Computer installing the {} software".format(software))

class SmartPhone(Phone,Computer):
    def __init__(self):
        Phone.__init__(self)        
        Computer.__init__(self)

class Tablet(Computer,Phone):
    def __init__(self):        
        Computer.__init__(self)
        Phone.__init__(self)       

class Phablet(SmartPhone,Tablet):
    def __init__(self):
        SmartPhone.__init__(self)
        Tablet.__init__(self)
```

如代码中所示，我创建了两个新类，一个是Tablet（多重继承自Computer和Phone，注意跟SmartPhone的不同），另个是Phablet（多重继承自SmartPhone和Tablet）。SmartPhone和Tablet对于基类Phone和Computer差生了交叉引用，Python解释器于是抛出了TypeError，因为C3线性化规则不能给出确切的基类的调用顺序。

```
>>> from mro_type_error import Phablet
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "mro_type_error.py", line 30, in <module>    class Phablet(SmartPhone, Tablet):
TypeError: Cannot create a consistent method resolution
order (MRO) for bases Phone, Computer
>>>
```
