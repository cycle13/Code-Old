---
layout:     post
title:      "Python 读取大文件"
subtitle:   "Python read large files"
date:       2017-10-01
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Python
---

如何使用Python读取1个8GB大小的文件,这个问题其实在笔试中会经常遇到的1个题目。对于在Python中读取文件的操作,一般我们会这样来操作:

```
f = open('filename','rb')
f.read()
```

下面我们来找1个比较大的文件,比如1个nginx的日志文件,记得之前有一次公司的1天的nginx日志文件解压为3GB大小,不得不对其进行切分。

这里我们找到了1个3G大小的文件。接下来,我们使用普通的读取方式来查看该文件的内容:

```
f=open('test','rb')
data=f.read()
---------------------------------------------------------------------------
MemoryError                               Traceback (most recent call last)
...
MemoryError:
```

我们可以看到1个MemoryError的错误,说明该无文件无法被装载在内存中发生溢出了。

下面我们来思考下为什么内存会溢出了,在我们打开文件的时候并没有发生任何异常,而在我们调用read方法时才出现问题。我们知道,文件对象的read方法会尝试将所有内容以1行的形式读入,显然这种方式对于大文件是不可行的。

在Python中,除了使用read方法读取文件内容外,还有另外2个方法readline和readlines也可以进行内容的读取。
既然默认read方法是一次性的将内容都读取到内存中,那么我们是否可以指定其每次读取的长度来解决这个问题呢?

```
data = f.read(1024)
while 1:
  #处理该行的代码
  data = f.read(1024)
```

而readlines会返回每1行读取的内容的列表,因此有一定风险的。

```
for l in f.readlines():
   #处理这1行的代码
```

那么,我们每次读取1行总可以了把。这样我们可以通过如下的方式来进行:

```
line = f.readline()
while 1:
    #处理该行的代码
    line = f.readline()
```

我们通过1个无限循环的方式来进行读取。结果发现,使用readlines的方式还是会导致内存不足的情况发生,而通过读取指定字节的方式则可以处理完这个文件。

在上面的解决方案中,我们需要手动处理文件读取的大小,并在合适的情况退出读取的操作。

那么,我们有没有更好的解决方案呢?实际上是有的,在Python的手册中,有1个xreadlines的方法,这个方法就类比range和xrange函数的区别。这个方法返回类似iter(f)的字符串,但是遗憾的是该方法在Python版本2.3中已经被淘汰了,官方建议我们使用for语句来替代:

```
for line in f:
    #处理该行的代码
```

通过这种方式,Python将处理文件对象为1个迭代器,并自动使用缓存IO和内存管理,这样我们就不需要关注大的文件了。
