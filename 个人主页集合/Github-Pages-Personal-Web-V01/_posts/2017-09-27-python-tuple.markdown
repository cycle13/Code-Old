---
layout:     post
title:      "Python元组小结"
subtitle:   "Python: tuple"
date:       2017-09-27
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

# 简介

1.元组是以圆括号“()”包围的数据集合，不同成员以“,”分隔。通过下标进行访问

2.不可变序列，可以看做不可变的列表，与列表不同：元组中数据一旦确立就不能改变（所以没有类似列表的增删改操作，只有基本序列操作）

3.支持任意类型，任意嵌套以及常见的序列操作

4.元组通常用在使语句或用户定义的函数能够安全地采用一组值的时候，即被使用的元组的值不会改变

# 声明及使用

```
t = () #空元组
t =(1,) #单个元素元组，注意逗号必须
t =(1,2,3)

1 in t #判断
2 not in t

#其他同序列基本操作：分片，索引
print t[0]
print t[-1]
print t[:2]

#不会对原来元组造成影响
print t+(4,5) #返回新元组(1,2,3,4,5)
print t * 2 #(1,2,3,1,2,3)
t.index(1)
t.count(1)

#列表元组转换
l = [1,2,3]
lt = tuple(l)
tl = list(lt)
lt_sorted = sorted(l) #对元组进行排序，返回是列表

#字符串转元组(得到字符元组序列)
print tuple('hello) #('h','e','l','l','o')
```

tuple没有append/extend/remove/pop等增删改操作

tuple没有find

查看帮助

```
help(tuple)
```

# 用途

1.赋值

```
t = 1,2,3 #等价 t = (1, 2, 3)
x, y, z = t #序列拆封，要求左侧变量数目和右侧序列长度相等
```

2.函数多个返回值

```
def test():
    return (1,2)

x, y = test()
```

3.传参[强制不改变原始序列]

```
def print_list(l):
    t = tuple(l) #或者t = l[:]
    dosomething()
```

4.字符串格式化

```
print '%s is %s years old' % ('tom', 20)
```

5.作为字典的key

# 优点

## 1.性能

tuple比列表操作速度快

若需要定义一个常量集，或者是只读序列，唯一的操作是不断遍历之，使用tuple代替list

```
>>> a = tuple(range(1000))
>>> b = range(1000)
>>> def test_t():
... for i in a:
... pass
...
>>> def test_l():
... for i in b:
... pass
...
>>> from timeit import Timer
>>> at = Timer("test_t()", "from __main__ import test_t")
>>> bt = Timer("test_l()", "from __main__ import test_l")
```

简单测试

```
>>> at.repeat(3, 100000)
[1.526214838027954, 1.5191287994384766, 1.5181210041046143]
>>> bt.repeat(3, 100000)
[1.5545141696929932, 1.557785987854004, 1.5511009693145752]
```

## 2.不可变性

对不需要的数据进行“写保护”，使代码更加安全

不可变性，若在程序中以列表形式传递对象集合，可能在任何地方被改变，使用元组，则不能

不可变性只适用于元组本身顶层而非其内容，例如元组内部的列表可以修改

```
l = [1,2,3]
t = (1,2,l)
l.append(4)
```

不可变性提供了某种完整性，规范化，确保不会被修改，保持某种固定关系

修改的方法

```
tuple -> list -> tuple
```

## 补充

元组定义易错点

一个和多个的区别是定义1个后面必须有”,”否则就是 类型 “str”

```
>>> t = ("a")
>>> type (t)

>>> t = ("a",)
>>> type (t)

>>> t = ("a","b")
>>> type (t)

>>> t = "a",
>>> type(t)
```
