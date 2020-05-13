---
layout:     post
title:      "Python 中的下划线"
subtitle:   "Underline in Python"
date:       2017-08-24
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

# 单个下划线（_）

## 1. 解释器中

_符号是指交互解释器中最后一次执行语句的返回结果。这种用法最初出现在CPython解释器中，其他解释器后来也都跟进了。

```
>>> _
Traceback (most recent call last):
  File "", line 1, in
NameError: name '_' is not defined
>>> 42
>>> _
42
>>> 'alright!' if _ else ':('
'alright!'
>>> _
'alright!'
```

## 2. 作为名称使用

这个跟上面有点类似。_用作被丢弃的名称。按照惯例，这样做可以让阅读你代码的人知道，这是个不会被使用的特定名称。举个例子，你可能无所谓一个循环计数的值：

```
n = 42
for _ in range(n):
    do_something()
```

## 3. i18n

_还可以被用作函数名。这种情况，单下划线经常被用作国际化和本地化字符串翻译查询的函数名。这种惯例好像起源于C语言。举个例子，在 Django documentation for translation 中你可能会看到：

```
from django.utils.translation import ugettext as _
from django.http import HttpResponse
 
def my_view(request):
    output = _("Welcome to my site.")
    return HttpResponse(output)
```

第二种和第三种用法会引起冲突，所以在任意代码块中，如果使用了_作i18n翻译查询函数，就应该避免再用作被丢弃的变量名。

# 单下划线前缀的名称（例如_shahriar）

以单下划线做前缀的名称指定了这个名称是“私有的”。在 有些 导入import * 的场景中，下一个使用你代码的人（或者你本人）会明白这个名称仅内部使用。Python documentation里面写道：

> a name prefixed with an underscore (e.g. _spam) should be treated as a non-public part of the API (whether it is a function, a method or a data member). It should be considered an implementation detail and subject to change without notice.

之所以说在在 有些 import * 的场景，是因为导入时解释器确实对单下划线开头的名称做了处理。如果你这么写from import *，任何以单下划线开头的名称都不会被导入，除非模块/包的__all__列表明确包含了这些名称。更多相关信息见““Importing * in Python”。

# 双下划线前缀的名称（例如__shahriar）

以双下划线做前缀的名称（特别是方法名）并不是一种惯例；它对解释器有特定含义。Python会改写这些名称，以免与子类中定义的名称产生冲突。Python documentation中提到，任何__spam这种形式（至少以两个下划线做开头，绝大部分都还有一个下划线做结尾）的标识符，都会文本上被替换为_classname__spam，其中classname是当前类名，并带上一个下划线做前缀。
看下面这个例子：

```
>>> class A(object):
...     def _internal_use(self):
...         pass
...     def __method_name(self):
...         pass
...
>>> dir(A())
['_A__method_name', ..., '_internal_use']
```

正如所料，_internal_use没有变化，但__method_name被改写成了_ClassName__method_name。现在创建一个A的子类B（这可不是个好名字），就不会轻易的覆盖掉A中的__method_name了：

```
>>> class B(A):
...     def __method_name(self):
...         pass
...
>>> dir(B())
['_A__method_name', '_B__method_name', ..., '_internal_use']
```

这种特定的行为差不多等价于Java中的final方法和C++中的正常方法（非虚方法）。

# 前后都带有双下划线的名称（例如__init__）

这些是Python的特殊方法名，这仅仅是一种惯例，一种确保Python系统中的名称不会跟用户自定义的名称发生冲突的方式。通常你可以覆写这些方法，在Python调用它们时，产生你想得到的行为。例如，当写一个类的时候经常会覆写__init__方法。

你也可以写出自己的“特殊方法”名（但是别这么做）：

```
>>> class C(object):
...     def __mine__(self):
...         pass
...
>>> dir(C)
... [..., '__mine__', ...]
```

还是不要这样写方法名，只让Python定义的特殊方法名使用这种惯例吧。