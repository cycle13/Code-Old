---
layout:     post
title:      "基本线性数据结构的Python实现"
subtitle:   "Python basic data structure"
date:       2017-10-21
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Python
    - 算法
---

本篇主要实现四种数据结构，分别是数组、堆栈、队列、链表。我不知道我为什么要用Python来干C干的事情，总之Python就是可以干。

# 数组

## 数组的设计

数组设计之初是在形式上依赖内存分配而成的，所以必须在使用前预先请求空间。这使得数组有以下特性：

1. 请求空间以后大小固定，不能再改变（数据溢出问题）；

2. 在内存中有空间连续性的表现，中间不会存在其他程序需要调用的数据，为此数组的专用内存空间；

3. 在旧式编程语言中（如有中阶语言之称的C），程序不会对数组的操作做下界判断，也就有潜在的越界操作的风险（比如会把数据写在运行中程序需要调用的核心部分的内存上）。

因为简单数组强烈倚赖电脑硬件之内存，所以不适用于现代的程序设计。欲使用可变大小、硬件无关性的数据类型，Java等程序设计语言均提供了更高级的数据结构：ArrayList、Vector等动态数组。

## Python的数组

从严格意义上来说:Python里没有严格意义上的数组。

List可以说是Python里的数组，下面这段代码是CPython的实现List的结构体:

```
typedef struct {
    PyObject_VAR_HEAD
    /* Vector of pointers to list elements.  list[0] is ob_item[0], etc. */
    PyObject **ob_item;

    /* ob_item contains space for 'allocated' elements.  The number
     * currently in use is ob_size.
     * Invariants:
     *     0 <= ob_size <= allocated
     *     len(list) == ob_size
     *     ob_item == NULL implies ob_size == allocated == 0
     * list.sort() temporarily sets allocated to -1 to detect mutations.
     *
     * Items must normally not be NULL, except during construction when
     * the list is not yet visible outside the function that builds it.
     */
    Py_ssize_t allocated;
} PyListObject;
```

当然，在Python里它就是数组。

后面的一些结构也将用List来实现。

# 堆栈

## 什么是堆栈

堆栈（英语：stack），也可直接称栈，在计算机科学中，是一种特殊的串列形式的数据结构，它的特殊之处在于只能允许在链接串列或阵列的一端（称为堆叠顶端指标，英语：top）进行加入资料（英语：push）和输出资料（英语：pop）的运算。另外堆叠也可以用一维阵列或连结串列的形式来完成。堆叠的另外一个相对的操作方式称为伫列。

由于堆叠数据结构只允许在一端进行操作，因而按照后进先出（LIFO, Last In First Out）的原理运作。

## 特点

* 先入后出，后入先出。

* 除头尾节点之外，每个元素有一个前驱，一个后继。

## 操作

从原理可知，对堆栈(栈)可以进行的操作有:

* top():获取堆栈顶端对象
* push():向栈里添加一个对象
* pop():从栈里推出一个对象

## 实现

```
class my_stack(object):
    def __init__(self, value):
        self.value = value
        # 前驱
        self.before = None
        # 后继
        self.behind = None

    def __str__(self):
        return str(self.value)


def top(stack):
    if isinstance(stack, my_stack):
        if stack.behind is not None:
            return top(stack.behind)
        else:
            return stack


def push(stack, ele):
    push_ele = my_stack(ele)
    if isinstance(stack, my_stack):
      stack_top = top(stack)
      push_ele.before = stack_top
      push_ele.before.behind = push_ele
    else:
      raise Exception('不要乱扔东西进来好么')


def pop(stack):
    if isinstance(stack, my_stack):
        stack_top = top(stack)
        if stack_top.before is not None:
            stack_top.before.behind = None
            stack_top.behind = None
            return stack_top
        else:
            print('已经是栈顶了')
```

# 队列

## 什么是队列

和堆栈类似，唯一的区别是队列只能在队头进行出队操作，所以队列是是先进先出（FIFO, First-In-First-Out）的线性表

## 特点

* 先入先出,后入后出
* 除尾节点外,每个节点有一个后继
* （可选）除头节点外,每个节点有一个前驱

## 操作

* push（）:入队
* pop（）：出队

## 实现

### 普通队列

```
class MyQueue():
    def __init__(self, value=None):
        self.value = value
        # 前驱
        # self.before = None
        # 后继
        self.behind = None

    def __str__(self):
        if self.value is not None:
            return str(self.value)
        else:
            return 'None'


def create_queue():
    """仅有队头"""
    return MyQueue()


def last(queue):
    if isinstance(queue, MyQueue):
        if queue.behind is not None:
            return last(queue.behind)
        else:
            return queue


def push(queue, ele):
    if isinstance(queue, MyQueue):
        last_queue = last(queue)
        new_queue = MyQueue(ele)
        last_queue.behind = new_queue


def pop(queue):
    if queue.behind is not None:
        get_queue = queue.behind
        queue.behind = queue.behind.behind
        return get_queue
    else:
        print('队列里已经没有元素了')

def print_queue(queue):
    print(queue)
    if queue.behind is not None:
        print_queue(queue.behind)
```

# 链表

## 什么是链表

链表（Linked list）是一种常见的基础数据结构，是一种线性表，但是并不会按线性的顺序存储数据，而是在每一个节点里存到下一个节点的指针(Pointer)。由于不必须按顺序存储，链表在插入的时候可以达到O(1)的复杂度，比另一种线性表顺序表快得多，但是查找一个节点或者访问特定编号的节点则需要O(n)的时间，而顺序表相应的时间复杂度分别是O(logn)和O(1)。

## 特点

使用链表结构可以克服数组链表需要预先知道数据大小的缺点，链表结构可以充分利用计算机内存空间，实现灵活的内存动态管理。但是链表失去了数组随机读取的优点，同时链表由于增加了结点的指针域，空间开销比较大。

## 操作

* init():初始化
* insert(): 插入
* trave(): 遍历
* delete(): 删除
* find(): 查找

## 实现

此处仅实现双向列表

```
class LinkedList():
    def __init__(self, value=None):
        self.value = value
        # 前驱
        self.before = None
        # 后继
        self.behind = None

    def __str__(self):
        if self.value is not None:
            return str(self.value)
        else:
            return 'None'


def init():
    return LinkedList('HEAD')


def delete(linked_list):
    if isinstance(linked_list, LinkedList):
        if linked_list.behind is not None:
            delete(linked_list.behind)
            linked_list.behind = None
            linked_list.before = None
        linked_list.value = None


def insert(linked_list, index, node):
    node = LinkedList(node)
    if isinstance(linked_list, LinkedList):
        i = 0
        while linked_list.behind is not None:
            if i == index:
                break
            i += 1
            linked_list = linked_list.behind
        if linked_list.behind is not None:
            node.behind = linked_list.behind
            linked_list.behind.before = node
        node.before, linked_list.behind = linked_list, node


def remove(linked_list, index):
    if isinstance(linked_list, LinkedList):
        i = 0
        while linked_list.behind is not None:
            if i == index:
                break
            i += 1
            linked_list = linked_list.behind
        if linked_list.behind is not None:
            linked_list.behind.before = linked_list.before
        if linked_list.before is not None:
            linked_list.before.behind = linked_list.behind
        linked_list.behind = None
        linked_list.before = None
        linked_list.value = None


def trave(linked_list):
    if isinstance(linked_list, LinkedList):
        print(linked_list)
        if linked_list.behind is not None:
            trave(linked_list.behind)


def find(linked_list, index):
    if isinstance(linked_list, LinkedList):
        i = 0
        while linked_list.behind is not None:
            if i == index:
                return linked_list
            i += 1
            linked_list = linked_list.behind
        else:
            if i < index:
                raise Exception(404)
            return linked_list
```
