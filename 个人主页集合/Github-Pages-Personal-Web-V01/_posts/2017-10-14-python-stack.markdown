---
layout:     post
title:      "Python 数据结构——栈"
subtitle:   "Python stack"
date:       2017-10-14
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Python
    - 算法
---

栈是一种特殊的列表，栈内的元素只能通过列表的一端访问，这一端称为栈顶。咖啡厅内的一摞盘子是现实世界中常见的栈的例子。只能从最上面取盘子，盘子洗净后，也只能摞在这一摞盘子的最上面。栈被称为一种后入先出（LIFO，last-in-first-out）的数据结构。

由于栈具有后入先出的特点，所以任何不在栈顶的元素都无法访问。为了得到栈底的元素，必须先拿掉上面的元素。

对栈的两种主要操作是将一个元素压入栈和将一个元素弹出栈。入栈使用push()方法，出栈使用pop()方法。下图演示了入栈和出栈的过程。

![img](/img/in-post/2017-10-14-python-stack/01.png)

另一个常用的操作是预览栈顶的元素。pop()方法虽然可以访问栈顶的元素，但是调用该方法后，栈顶元素也从栈中被永久性地删除了。peek()方法则只返回栈顶元素，而不删除它。

为了记录栈顶元素的位置，同时也为了标记哪里可以加入新元素，我们使用变量top，当向栈内压入元素时，该变量增大；从栈内弹出元素时，该变量减小。

push()、pop()和peek()是栈的3个主要方法，但是栈还有其他方法和属性。

stack通常的操作：

```
Stack()    建立一个空的栈对象
push()     把一个元素添加到栈的最顶层
pop()      删除栈最顶层的元素，并返回这个元素
peek()     返回最顶层的元素，并不删除它
isEmpty()  判断栈是否为空
size()     返回栈中元素的个数
```

简单案例以及操作结果：

![img](/img/in-post/2017-10-14-python-stack/02.png)

这里使用python的list对象模拟栈的实现：

```
class Stack:
    """模拟栈"""
    def __init__(self):
        self.items = []
 
    def isEmpty(self):
        return len(self.items)==0  
 
    def push(self, item):
        self.items.append(item)
 
    def pop(self):
        return self.items.pop()  
 
    def peek(self):
        if not self.isEmpty():
            return self.items[len(self.items)-1]
 
    def size(self):
        return len(self.items)
```

创建一个栈对象，并加入操作方法：

```
s=Stack()
print(s.isEmpty())
s.push(4)
s.push('dog')
print(s.peek())
s.push(True)
print(s.size())
print(s.isEmpty())
s.push(8.4)
print(s.pop())
print(s.pop())

print(s.size())
```