---
layout:     post
title:      "pickle 和 cPickle：Python 对象的序列化"
subtitle:   "pickle and cPickle"
date:       2017-10-18
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

目的：Python对象序列化

可用性：pickle至少1.4版本，cPickle 1.5版本以上


pickle模块实现了一种算法，将任意一个Python对象转化成一系列字节（byets）。此过程也调用了serializing对象。代表对象的字节流之后可以被传输或存储，再重构后创建一个拥有相同特征（the same characteristics）的新的对象。

cPickle使用C而不是Python，实现了相同的算法。这比Python实现要快好几倍，但是它不允许用户从Pickle派生子类。如果子类对你的使用来说无关紧要，那么cPickle是个更好的选择。

警告:本文档直接说明，pickle不提供安全保证。如果你在多线程通信（inter-process communication）或者数据存储或存储数据中使用pickle，一定要小心。请勿信任你不能确定为安全的数据。

# 导入

如平常一样，尝试导入cPickle，给它赋予一个别名“pickle”。如果因为某些原因导入失败，退而求其次到Python的原生（native）实现pickle模块。如果cPickle可用，能给你提供一个更快速的执行，否则只能是轻便的执行（the portable implementation）。

```
try:
   import cPickle as pickle
except:
   import pickle
```

# 编码和解码

第一个例子将一种数据结构编码成一个字符串，然后把该字符串打印至控制台。使用一种包含所有原生类型（native types）的数据结构。任何类型的实例都可被腌渍（pickled，译者注：模块名称pickle的中文含义为腌菜），在稍后的例子中会演示。使用pickle.dumps()来创建一个表示该对象值的字符串。

```
try:
    import cPickle as pickle
except:
    import pickle
import pprint

data = [ { 'a':'A', 'b':2, 'c':3.0 } ]
print 'DATA:',
pprint.pprint(data)

data_string = pickle.dumps(data)
print 'PICKLE:', data_string
```

pickle默认仅由ASCII字符组成。也可以使用更高效的二进制格式（binary format），只是因为在打印的时候更易于理解，本页的所有例子都使用ASCII输出。

```
$ python pickle_string.py

DATA:[{'a': 'A', 'b': 2, 'c': 3.0}]
PICKLE: (lp1
(dp2
S'a'
S'A'
sS'c'
F3
sS'b'
I2
sa.
```

数据被序列化以后，你可以将它们写入文件、套接字、管道等等中。之后你也可以从文件中读取出来、将它反腌渍（unpickled）而构造一个具有相同值得新对象。

```
try:
    import cPickle as pickle
except:
    import pickle
import pprint

data1 = [ { 'a':'A', 'b':2, 'c':3.0 } ]
print 'BEFORE:',
pprint.pprint(data1)

data1_string = pickle.dumps(data1)

data2 = pickle.loads(data1_string)
print 'AFTER:',
pprint.pprint(data2)

print 'SAME?:', (data1 is data2)
print 'EQUAL?:', (data1 == data2)
```

如你所见，这个新构造的对象与原对象相同，但并非同一对象。这不足为奇。

```
$ python pickle_unpickle.py

BEFORE:[{'a': 'A', 'b': 2, 'c': 3.0}]
AFTER:[{'a': 'A', 'b': 2, 'c': 3.0}]
SAME?: False
EQUAL?: True
```

# 与流一起工作

除dumps()和loads()外，pickle还提供一对用在类文件流（file-like streams）的转化函数。可以往一个流中写对个对象，然后从流中把它们读取出来，此过程不需要预先写入的对象有几个、它们多大。

```
try:
    import cPickle as pickle
except:
    import pickle
import pprint
from StringIO import StringIO

class SimpleObject(object):

    def __init__(self, name):
        self.name = name
        l = list(name)
        l.reverse()
        self.name_backwards = ''.join(l)
        return

data = []
data.append(SimpleObject('pickle'))
data.append(SimpleObject('cPickle'))
data.append(SimpleObject('last'))

# 使用StringIO模拟一个文件
out_s = StringIO()

# 写入该流
for o in data:
    print 'WRITING: %s (%s)' % (o.name, o.name_backwards)
    pickle.dump(o, out_s)
    out_s.flush()

# 建立一个可读流
in_s = StringIO(out_s.getvalue())

# 读数据
while True:
    try:
        o = pickle.load(in_s)
    except EOFError:
        break
    else:
        print 'READ: %s (%s)' % (o.name, o.name_backwards)
```

这个例子使用SringIO缓存器（buffer）模拟流，所以在建立可读流的时候我们玩了一把。一个简单数据库的格式化也可以使用pickles来存储对象，只是shelve与之工作更加简便。

```
$ python pickle_stream.py

WRITING: pickle (elkcip)
WRITING: cPickle (elkciPc)
WRITING: last (tsal)
READ: pickle (elkcip)
READ: cPickle (elkciPc)
READ: last (tsal)
```

除了存储数据，pickles在进程间通信（inter-process communication）中也非常称手。例如，使用os.fork()和os.pipe()可以创立工作者进程（worker processes），从一个管道（pipe）读取作业指令（job instruction）然后将结果写入另一个管道。管理工作者池（worker pool）和将作业送入、接受响应（response）的核心代码可被重用，因为作业和响应并不属于某个特定类中。如果你使用管道或者套接字（sockets），在通过连至另一端（end）的连接倾倒（dumps）所有对象、推送数据之后，别忘了冲洗（flush）。如果你想写自己的工作者池管理器，请看multiprocessing

# 重构对象的问题

当与你自己的类一起工作时，你必须保证类被腌渍出现在读取pickle的进程的命名空间中。只有该实例的数据而不是类定义被腌渍。类名被用于在反腌渍时，找到构造器（constructor）以创建新对象。以此——往一个文件写入一个类的实例为例：

```
try:
    import cPickle as pickle
except:
    import pickle
import sys

class SimpleObject(object):

    def __init__(self, name):
        self.name = name
        l = list(name)
        l.reverse()
        self.name_backwards = ''.join(l)
        return

if __name__ == '__main__':
    data = []
    data.append(SimpleObject('pickle'))
    data.append(SimpleObject('cPickle'))
    data.append(SimpleObject('last'))

    try:
        filename = sys.argv[1]
    except IndexError:
        raise RuntimeError('Please specify a filename as an argument to %s' % sys.argv[0])

    out_s = open(filename, 'wb')
    try:
        # 写入流中
        for o in data:
            print 'WRITING: %s (%s)' % (o.name, o.name_backwards)
            pickle.dump(o, out_s)
    finally:
        out_s.close()
```

在运行时，该脚本创建一个以在命令行指定的参数为名的文件：

```
$ python pickle_dump_to_file_1.py test.dat

WRITING: pickle (elkcip)
WRITING: cPickle (elkciPc)
WRITING: last (tsal)
```

一个在读取结果腌渍对象失败的简化尝试：

```
try:
    import cPickle as pickle
except:
    import pickle
import pprint
from StringIO import StringIO
import sys


try:
    filename = sys.argv[1]
except IndexError:
    raise RuntimeError('Please specify a filename as an argument to %s' % sys.argv[0])

in_s = open(filename, 'rb')
try:
    # 读取数据
    while True:
        try:
            o = pickle.load(in_s)
        except EOFError:
            break
        else:
            print 'READ: %s (%s)' % (o.name, o.name_backwards)
finally:
    in_s.close()
```

该版本失败的原因在于没有 SimpleObject 类可用：

```
$ python pickle_load_from_file_1.py test.dat

Traceback (most recent call last):
  File "pickle_load_from_file_1.py", line 52, in <module>
    o = pickle.load(in_s)
AttributeError: 'module' object has no attribute 'SimpleObject'
```

正确的版本从原脚本中导入 SimpleObject ，可成功运行。

添加：

```
from pickle_dump_to_file_1 import SimpleObject
```

至导入列表的尾部，接着重新运行该脚本：

```
$ python pickle_load_from_file_2.py test.dat

READ: pickle (elkcip)
READ: cPickle (elkciPc)
READ: last (tsal)
```

当腌渍有值的数据类型不能被腌渍时（套接字、文件句柄（file handles）、数据库连接等之类的），有一些特别的考虑。因为使用值而不能被腌渍的类，可以定义 `__getstate__()` 和 `__setstate__()` 来返回状态（state）的一个子集，才能被腌渍。新式类（New-style classes）也可以定义`__getnewargs__()`，该函数应当返回被传递至类内存分配器（the class memory allocator）（`C.__new__()`）的参数。使用这些新特性的更多细节，包含在标准库文档中。

# 环形引用（Circular References）

pickle协议（pickle protocol）自动处理对象间的环形引用，因此，即使是很复杂的对象，你也不用特别为此做什么。考虑下面这个图：

![img](/img/in-post/2017-10-18-pickle-and-cPickle/01.png)

上图虽然包括几个环形引用，但也能以正确的结构腌渍和重新读取（reloaded）。

```
import pickle

class Node(object):
    """
    一个所有结点都可知它所连通的其它结点的简单有向图。
    """
    def __init__(self, name):
        self.name = name
        self.connections = []
        return

    def add_edge(self, node):
        "创建两个结点之间的一条边。"
        self.connections.append(node)
        return

    def __iter__(self):
        return iter(self.connections)

def preorder_traversal(root, seen=None, parent=None):
    """产生器（Generator ）函数通过一个先根遍历（preorder traversal）生成（yield）边。"""
    if seen is None:
        seen = set()
    yield (parent, root)
    if root in seen:
        return
    seen.add(root)
    for node in root:
        for (parent, subnode) in preorder_traversal(node, seen, root):
            yield (parent, subnode)
    return

def show_edges(root):
    "打印图中的所有边。"
    for parent, child in preorder_traversal(root):
        if not parent:
            continue
        print '%5s -> %2s (%s)' % (parent.name, child.name, id(child))

# 创建结点。
root = Node('root')
a = Node('a')
b = Node('b')
c = Node('c')

# 添加边。
root.add_edge(a)
root.add_edge(b)
a.add_edge(b)
b.add_edge(a)
b.add_edge(c)
a.add_edge(a)

print 'ORIGINAL GRAPH:'
show_edges(root)

# 腌渍和反腌渍该图来创建
# 一个结点集合。
dumped = pickle.dumps(root)
reloaded = pickle.loads(dumped)

print
print 'RELOADED GRAPH:'
show_edges(reloaded)
```

重新读取的诸多节点（译者注：对应图中的圆圈）不再是同一个对象，但是节点间的关系保持住了，而且读取的仅仅是带有多个引用的对象的一个拷贝。上面所说的可以通过测试各节点在pickle处理前和之后的id()值来验证。

```
$ python pickle_cycle.py

ORIGINAL GRAPH:
root ->  a (4299721744)
    a ->  b (4299721808)
    b ->  a (4299721744)
    b ->  c (4299721872)
    a ->  a (4299721744)
root ->  b (4299721808)

RELOADED GRAPH:
root ->  a (4299722000)
    a ->  b (4299722064)
    b ->  a (4299722000)
    b ->  c (4299722128)
    a ->  a (4299722000)
root ->  b (4299722064)
```
