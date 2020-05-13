---
layout:     post
title:      "简析 __init__、__new__、__call__ 方法"
subtitle:   "Simple analysis of __init__、__new__、__call__"
date:       2017-10-11
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

任何事物都有一个从创建，被使用，再到消亡的过程，在程序语言面向对象编程模型中，对象也有相似的命运：创建、初始化、使用、垃圾回收，不同的阶段由不同的方法（角色）负责执行。

定义一个类时，大家用得最多的就是 `__init__` 方法，而 `__new__` 和 `__call__` 使用得比较少，这篇文章试图帮助大家把这3个方法的正确使用方式和应用场景分别解释一下。

关于 Python 新式类和老式类在这篇文章不做过多讨论，因为老式类是 Python2 中的概念，现在基本没人再会去用老式类，新式类必须显示地继承 object，而 Python3 中，只有新式类，默认继承了 object，无需显示指定，本文代码都是基于 Python3 来讨论。

# `__init__`

`__init__`方法负责对象的初始化，系统执行该方法前，其实该对象已经存在了，要不然初始化什么东西呢？先看例子：

```
# class A(object): python2 必须显示地继承object
class A:
    def __init__(self):
        print("__init__ ")
        super(A, self).__init__()

    def __new__(cls):
        print("__new__ ")
        return super(A, cls).__new__(cls)

    def __call__(self):  # 可以定义任意参数
        print('__call__ ')

A()
```

输出

```
__new__
__init__
```

从输出结果来看， `__new__`方法先被调用，返回一个实例对象，接着 `__init__` 被调用。 `__call__` 方法并没有被调用，这个我们放到最后说，先来说说前面两个方法，稍微改写成：

```
def __init__(self):
    print("__init__ ")
    print(self)
    super(A, self).__init__()

def __new__(cls):
    print("__new__ ")
    self = super(A, cls).__new__(cls)
    print(self)
    return self
```

输出：

```
__new__ 
<__main__.A object at 0x1007a95f8>
__init__ 
<__main__.A object at 0x1007a95f8>
```

从输出结果来看，`__new__` 方法的返回值就是类的实例对象，这个实例对象会传递给 `__init__` 方法中定义的 self 参数，以便实例对象可以被正确地初始化。

如果 `__new__` 方法不返回值（或者说返回 None）那么 `__init__` 将不会得到调用，这个也说得通，因为实例对象都没创建出来，调用 init 也没什么意义，此外，Python 还规定，`__init__` 只能返回 None 值，否则报错，这个留给大家去试。

`__init__` 方法可以用来做一些初始化工作，比如给实例对象的状态进行初始化：

```
def __init__(self, a, b):
    self.a = a
    self.b = b
    super(A, self).__init__()
```

# `__new__`

一般我们不会去重写该方法，除非你确切知道怎么做，什么时候你会去关心它呢，它作为构造函数用于创建对象，是一个工厂函数，专用于生产实例对象。著名的设计模式之一，单例模式，就可以通过此方法来实现。在自己写框架级的代码时，可能你会用到它，我们也可以从开源代码中找到它的应用场景，例如微型 Web 框架 Bootle 就用到了。

```
class BaseController(object):
    _singleton = None
    def __new__(cls, *a, **k):
        if not cls._singleton:
            cls._singleton = object.__new__(cls, *a, **k)
        return cls._singleton
```

这段代码出自 https://github.com/bottlepy/bottle/blob/release-0.6/bottle.py

这就是通过 `__new__` 方法是实现单例模式的的一种方式，如果实例对象存在了就直接返回该实例即可，如果还没有，那么就先创建一个实例，再返回。当然，实现单例模式的方法不只一种，Python之禅有说：

> There should be one— and preferably only one —obvious way to do it.

>用一种方法，最好是只有一种方法来做一件事

# `__call__`

关于 `__call__` 方法，不得不先提到一个概念，就是可调用对象（callable），我们平时自定义的函数、内置函数和类都属于可调用对象，但凡是可以把一对括号()应用到某个对象身上都可称之为可调用对象，判断对象是否为可调用对象可以用函数 callable

如果在类中实现了 `__call__` 方法，那么实例对象也将成为一个可调用对象，我们回到最开始的那个例子：

```
a = A()
print(callable(a))  # True
```

a是实例对象，同时还是可调用对象，那么我就可以像函数一样调用它。试试：

```
a()  # __call__
```

很神奇不是，实例对象也可以像函数一样作为可调用对象来用，那么，这个特点在什么场景用得上呢？这个要结合类的特性来说，类可以记录数据（属性），而函数不行（闭包某种意义上也可行），利用这种特性可以实现基于类的装饰器，在类里面记录状态，比如，下面这个例子用于记录函数被调用的次数：

```
class Counter:
    def __init__(self, func):
        self.func = func
        self.count = 0

    def __call__(self, *args, **kwargs):
        self.count += 1
        return self.func(*args, **kwargs)

@Counter
def foo():
    pass

for i in range(10):
    foo()

print(foo.count)  # 10
```

在 Bottle 中也有 call 方法 的使用案例，另外，stackoverflow 也有一些关于 call 的实践例子，推荐看看，如果你的项目中，需要更加抽象化、框架代码，那么这些高级特性往往能发挥出它作用。