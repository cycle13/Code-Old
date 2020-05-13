---
layout:     post
title:      "Python yield"
subtitle:   "Python yield"
date:       2017-10-14
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

# 1、迭代器

迭代器是访问集合内元素的一种方式。迭代器对象从集合的第一个元素开始访问，直到所有元素都被访问一遍后结束。迭代器不能回退，只能向前进行迭代。

Python中最常使用迭代器的场景是循环语句 for

```
>>> for i in range(5): #range返回一个列表
	print i

	
0
1
2
3
4
```

其中的range()返回一个包含所有指定元素的集合，而for语句将其封装成一个迭代器后访问。使用iter()调用可以将列表、集合转换为迭代器。

```
>>> a = [1,3,5,7,9]
>>> t = iter(a)
>>> print t
<list_iterator object at 0x0000000003435240>
```

代码中t即迭代器。

迭代器与普通Python对象的区别是迭代器有一个next()方法，每次调用该方法可以返回一个元素。调用者（比如for语句）可以通过不断调用next()方法来组个访问几何元素。

```
>>> iter = iter(range(5))
>>> print iter.next()
0
>>> print iter.next()
1
>>> print iter.next()
2
>>> print iter.next()
3
>>> print iter.next()
4
>>> print iter.next()

Traceback (most recent call last):
  File "<pyshell#6>", line 1, in <module>
    print iter.next()
StopIteration
```

> 注：Python3中用法不一样.

# 2、使用yield

迭代器在Python中应用很广，开发者可以使用yield定制自己的迭代器。

```
# -*- coding:utf-8 -*-

def demoIterator():
    print "I'm in the first call of next()"
    yield 1
    print "I'm in the second call of next()"
    yield 3
    print "I'm in the third call of next()"
    yield 9


for i in demoIterator():
    print i
```

结果为

```
I'm in the first call of next()
1
I'm in the second call of next()
3
I'm in the third call of next()
9
```

每次调用迭代器的next()函数，将会执行迭代器函数，并返回yield的结果作为迭代返回元素。当迭代器函数return时，迭代器会抛出StopIteration 异常使迭代终止。

> 技巧：在Python中，使用yield关键字定义的迭代器也被成为“生成器”。

# 3、实例

说了那么多太抽象，举例来看看。

yield 实现Fibonacci数列（斐波那契数列）

> 斐波那契数列指的是这样一个数列 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233，377，610，987，1597，2584，4181，6765，10946，17711，28657，46368........这个数列从第3项开始，每一项都等于前两项之和。

```
def fab(max):  
n, a, b = 0, 0, 1  
while n < max:  
        yield b  
        # print b  
        a, b = b, a + b  
        n = n + 1
```

执行 fab(5)，我们可以得到如下输出：

```
>>> fab(5) 
 1 
 1 
 2 
 3 
 5
```