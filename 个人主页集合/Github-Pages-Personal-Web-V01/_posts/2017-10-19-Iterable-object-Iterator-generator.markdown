---
layout:     post
title:      "可迭代对象 vs 迭代器 vs 生成器"
subtitle:   "Iterable object vs Iterator vs Generator"
date:       2017-10-19
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

在使用Python的过程中，很容易混淆如下几个关联的概念：

* 容器(container)
* 可迭代对象(Iterable)
* 迭代器(Iterator)
* 生成器(generator)
* 生成器表达式
* {list, set, dict} 解析式

它们之间的关系如下表所示：

![img](/img/in-post/2017-10-19-Iterable-object-Iterator-generator/01.png)

# 容器（container)

容器是用来储存元素的一种数据结构，它支持隶属测试，容器将所有数据保存在内存中，在Python中典型的容器有：


* list， deque, …
* set，frozesets，…
* dict, defaultdict, OrderedDict, Counter, …
* tuple, namedtuple, …
* str


容器相对来说很好理解，因为你可以把它当成生活中的箱子、房子、船等等。
一般的，通过判断一个对象是否包含某个元素来确定它是否为一个容器。例如：

```
>>> assert 1 in [1,2,3]       # lists
>>> assert 4 not in [1,2,3]
>>> assert 1 in {1,2,3}       # sets
>>> assert 4 not in {1,2,3}
>>> assert 1 in (12,3)        # tuples
>>> assert 4 not in (1,2,3)
```

字典容器通过检查是否包含键来进行判断：

```
>>> d = {1:"foo", 2:"bar", 3:"qux"}
>>> assert 1 in d
>>> assert 4 not in d
>>> assert "foo" not in d
```

字符串通过检查是否包含某个子 串来判断：

```
>>> s ="foobar"
>>> assert "b" in s
>>> assert "x" not in s
>>> assert "foo" in s
```

注意：并非所有的容器都是可迭代对象。

# 可迭代对象

正如前面所提到的，大部分容器都是可迭代的，但是还有其他一些对象也可以迭代，例如，文件对象以及管道对象等等，容器一般来说存储的元素是有限的，同样的，可迭代对象也可以用来表示一个包含有限元素的数据结构。


可迭代对象可以为任意对象，不一定非得是基本数据结构，只要这个对象可以返回一个iterator。听起来可能有点费解，但是可迭代对象与迭代器之间有一个显著的区别。先看下面的例子：

```
>>> x = [1,2,3]
>>> y = iter(x）
>>> z = iter(x)
>>> next(y)
1
>>> next(y)
2
>>> next(z)
1
>>> type(x)
<class 'list'>
>>> type(y)
<class 'list_iterator'>
```

在这里，x是可迭代对象，而y和z都是迭代器，它们从可迭代对象x中获取值。

注意：可迭代的类中，一般实现以下两个方法，`__iter__()`以及`__next()__`方法，`__iter__()`方法返回self。

当我们运行以下代码的时候:

```
x = [1,2,3]
for elem in x:
     ...
```

实际调用过程如下：

![img](/img/in-post/2017-10-19-Iterable-object-Iterator-generator/02.png)


当我们反向编译这段代Python码的时候，可以发现它显示调用了 GET_ITER，本质上跟调用iter(x)一样，而FOR_ITER指令相等于调用next()方法来获取每个元素。

```
>>> import dis
>>> x = [1, 2, 3]
>>> dis.dis('for _ in x: pass')
1     0 SETUP_LOOP                     14 (to 17)
       3 LOAD_NAME                       0 (x)
       6 GET_ITER
>> 7 FOR_ITER                            6 (to 16)
       10 STORE_NAME                    1 (_)
       13 JUMP_ABSOLUTE               7
>> 16 POP_BLOCK
>> 17 LOAD_CONST                    0 (None)
       20 RETURN_VALUE
```

# 迭代器(Iterators)

那么什么是迭代器呢？任何具有`__next__()`方法的对象都是迭代器，对迭代器调用next()方法可以获取下一个值。而至于它使如何产生这个值的，跟它能否成为一个迭代器并没有关系。


所以迭代器本质上是一个产生值的工厂，每次向迭代器请求下一个值，迭代器都会进行计算出相应的值并返回。


迭代器的例子很多，例如，所有itertools模块中的函数都会返回一个迭代器，有的还可以产生无穷的序列。

```
>>> from itertools import count
>>> counter = count(start=13)
>>> next(counter)
13
>>> next（counter)
14
```

有的函数根据有限序列中生成无限序列：

```
>>> from itertools import cycle
>>> colors = cycle(["red","white","blue"])
>>> next(colors)
"red"
>>> next(colors）
"white"
>>> next(colors)
"blue"
>>> next(colors)
"red"
```

有的函数根据无限序列中生成有限序列：

```
>>> from itertools import islice
>>> colors = cycle(['red', 'white', 'blue'])               # infinite
>>> limited = islice(colors, 0, 4)                          # finite
>>> for x in limited:                                            # so safe to use for-loop on
...            print(x)
red
white
blue
red
```

为了更好的理解迭代器的内部结构，我们先来定义一个生成斐波拉契数的迭代器：

```
>>> class fib:
...             def __init__(self):
...                   self.prev = 0
...                   self.curr = 1
...
...             def __iter__(self):
...                   return self
...
...             def __next__(self):
...                   value = self.curr
...                   self.curr += self.prev
...                   self.prev = value
...                   return value
...
>>> f = fib()
>>> list(islice(f, 0, 10))
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

注意这个类既是可迭代的 （因为具有`__iter__()`方法），也是它自身的迭代器(因为具有`__next__()`方法)。


迭代器内部状态保存在当前实例对象的prev以及cur属性中，在下一次调用中将使用这两个属性。每次调用next()方法都会执行以下两步操作：

1. 修改状态，以便下次调用next()方法
2. 计算当前调用的结果

比喻：从外部来看，迭代器就像政府工作人员一样，没人找他办事的时候（请求值），工作人员就闲着，当有人来找他的时候（请求值），工作人员就会忙一会，把请求的东西找出来交给请求的人。忙完之后，又没事了，继续闲着。

# 生成器

生成器其实就是一种特殊的迭代器。它使一种更为高级、更为优雅的迭代器。
使用生成器让我们可以以一种更加简洁的语法来定义迭代器。
让我们先明确以下两点：


* 任意生成器都是迭代器（反过来不成立）
* 任意生成器，都是一个可以延迟创建值的工厂


下面也是一个生成斐波那契序列的工厂函数，不过是以生成器的方式编写的：

```
>>> def fib():
...            prev, curr = 0, 1
...            while True:
...                     yield curr
...                     prev, curr = curr, prev + curr
...
>>> f = fib()
>>> list(islice(f, 0, 10))
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

上面的代码是不是既优雅又简洁？注意其中用到的魔法关键字`yield`

一起来剖析下上面的代码：首先，fib其实是一个很普通的函数，但是函数中没有return语句，函数的返回值是一个生成器。


当调用f = fib()时，生成器被实例化并返回，这时并不会执行任何代码，生成器处于空闲状态，注意这里prev, curr = 0, 1并未执行。


然后这个生成器被包含在isslice()中，而这又是一个迭代器，所以还是没有执行上面的代码。


然后这个迭代器又被包含在list()中，它会根据传进来的参数生成一个列表。所以它首先对isslice()对象调用next()方法，isslice()对象又会对实例f调用next()。
我们来看其中的一步操作，在第一次调用中，会执行prev, curr = 0, 1, 然后进入while循环，当遇到yield curr的时候，返回当前curr值，然后又进入空闲状态。
生成的值传递给外层的isslice()，也相应生成一个值，然后传递给外层的list()，外层的list将这个值1添加到列表中。
然后继续执行后面的九步操作，每步操作的流程都是一样的。


然后执行到底11步的时候，isslice()对象就会抛出StopIteration异常，意味着已经到达末尾了。注意生成器不会接收到第11次next()请求，后面会被垃圾回收掉。

# 生成器的类型

在Python中两种类型的生成器：生成器函数以及生成器表达式。生成器函数就是包含yield参数的函数。生成器表达式与列表解析式类似。


假设使用如下语法创建一个列表：


```
>>> numbers = [1, 2, 3, 4, 5, 6]
>>> [x * x for x in numbers]
[1, 4, 9, 16, 25, 36]
```

使用set解析式也可以达到同样的目的：

```
>>> {x * x for x in numbers}{1, 4, 36, 9, 16, 25}
```

或者dict解析式:


```
>>> {x: x * x for x in numbers}
{1: 1, 2: 4, 3: 9, 4: 16, 5: 25, 6: 36}
```

还可以使用生成器表达式:

```
>>> lazy_squares = (x * x for x in numbers)
>>> lazy_squares
<generator object <genexpr> at 0x10d1f5510>
>>> next(lazy_squares)
1
>>> list(lazy_squares)
[4, 9, 16, 25, 36]
```

注意我们第一次调用next()之后，lazy_squares对象的状态已经发生改变，所以后面后面地调用list()方法只会返回部分元素组成的列表。

# 总结

生成器是Python中一种非常强大的特性，它让我们能够编写更加简洁的代码，同时也更加节省内存，使用CPU也更加高效。


使用生成器的小提示：在你的代码中找到与下面代码类似的地方：

```
def something():
      result = []
      for ... in ...:
           result.append(x)
      return result
```

用以下代码进行替换：

```
def iter_something():
      for ... in ...:
            yield x
```
