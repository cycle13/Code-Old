---
layout:     post
title:      "Python 中何时使用断言？"
subtitle:   "When shall we use assert in Python?"
date:       2017-09-27
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Python
---

这个问题是如何在一些场景下使用断言表达式，通常会有人误用它，所以我决定写一篇文章来说明何时使用断言，什么时候不用。

为那些还不清楚它的人，Python的assert是用来检查一个条件，如果它为真，就不做任何事。如果它为假，则会抛出AssertError并且包含错误信息。例如：

```
py> x = 23
py> assert x > 0, "x is not zero or negative"
py> assert x%2 == 0, "x is not an even number"
Traceback (most recent call last):
File "", line 1, in
AssertionError: x is not an even number
```

很多人用assert作为一个很快和容易的方法来在参数错误的时候抛出异常。但这样做是错的，非常错误，有两个原因。首先AssertError不是在测试参数时应该抛出的错误。你不应该像这样写代码：

```
if not isinstance(x, int):
raise AssertionError("not an int")
```

你应该抛出TypeError的错误，assert会抛出错误的异常。


但是，更危险的是，有一个关于assert的困扰：它可以被编译好然后从来不执行，如果你用 –O 或 –oo 选项运行Python，结果不保证assert表达式会运行到。当适当的使用assert时，这是未来，但是当assert不恰当的使用时，它会让代码用-O执行时出错。


那什么时候应该使用assert？没有特定的规则，断言应该用于：

* 防御型的编程
* 运行时检查程序逻辑
* 检查约定
* 程序常量
* 检查文档

（在测试代码的时候使用断言也是可接受的，是一种很方便的单元测试方法，你接受这些测试在用-O标志运行时不会做任何事。我有时在代码里使用assert False来标记没有写完的代码分支，我希望这些代码运行失败。尽管抛出NotImplementedError可能会更好。）


关于断言的意见有很多，因为它能确保代码的正确性。如果你确定代码是正确的，那么就没有用断言的必要了，因为他们从来不会运行失败，你可以直接移除这些断言。如果你确定检查会失败，那么如果你不用断言，代码就会通过编译并忽略你的检查。


在以上两种情况下会很有意思，当你比较肯定代码但是不是绝对肯定时。可能你会错过一些非常古怪的情况。在这个情况下，额外的运行时检查能帮你确保任何错误都会尽早地被捕捉到。


另一个好的使用断言的方式是检查程序的不变量。一个不变量是一些你需要依赖它为真的情况，除非一个bug导致它为假。如果有bug，最好能够尽早发现，所以我们为它进行一个测试，但是又不想减慢代码运行速度。所以就用断言，因为它能在开发时打开，在产品阶段关闭。

一个非变量的例子可能是，如果你的函数希望在它开始时有数据库的连接，并且承诺在它返回的时候仍然保持连接，这就是函数的不变量：

```
def some_function(arg):
    assert not DB.closed()
    ... # code goes here
    assert not DB.closed()
    return result
```

断言本身就是很好的注释，胜过你直接写注释：

```
# when we reach here, we know that n > 2
```

你可以通过添加断言来确保它：

```
assert n > 2
```

断言也是一种防御型编程。你不是让你的代码防御现在的错误，而是防止在代码修改后引发的错误。理想情况下，单元测试可以完成这样的工作，可是需要面对的现实是，它们通常是没有完成的。人们可能在提交代码前会忘了运行测试代码。有一个内部检查是另一个阻挡错误的防线，尤其是那些不明显的错误，却导致了代码出问题并且返回错误的结果。


加入你有一些if…elif 的语句块，你知道在这之前一些需要有一些值：

```
# target is expected to be one of x, y, or z, and nothing else.
if target == x:
    run_x_code()
elif target == y:
    run_y_code()
else:
    run_z_code()
```

假设代码现在是完全正确的。但它会一直是正确的吗？依赖的修改，代码的修改。如果依赖修改成 target = w 会发生什么，会关系到run_w_code函数吗？如果我们改变了代码，但没有修改这里的代码，可能会导致错误的调用 run_z_code 函数并引发错误。用防御型的方法来写代码会很好，它能让代码运行正确，或者立马执行错误，即使你在未来对它进行了修改。


在代码开头的注释很好的一步，但是人们经常懒得读或者更新注释。一旦发生这种情况，注释会变得没用。但有了断言，我可以同时对代码块的假设书写文档，并且在它们违反的时候触发一个干净的错误

```
assert target in (x, y, z)
if target == x:
    run_x_code()
elif target == y:
    run_y_code()
else:
    assert target == z
    run_z_code()
```

这样，断言是一种防御型编程，同时也是一种文档。我想到一个更好的方案：

```
if target == x:
    run_x_code()
elif target == y:
    run_y_code()
elif target == z:
    run_z_code()
else:
    # This can never happen. But just in case it does...
    raise RuntimeError("an unexpected error occurred")
```

按约定进行设计是断言的另一个好的用途。我们想象函数与调用者之间有个约定，比如下面的：


“如果你传给我一个非空字符串，我保证传会字符串的第一个字母并将其大写。”


如果约定被函数或调用这破坏，代码就会出问题。我们说函数有一些前置条件和后置条件，所以函数就会这么写：

```
def first_upper(astring):
    assert isinstance(astring, str) and len(astring) > 0
    result = astring[0].upper()
    assert isinstance(result, str) and len(result) == 1
    assert result == result.upper()
    return result
```

按约定设计的目标是为了正确的编程，前置条件和后置条件是需要保持的。这是断言的典型应用场景，因为一旦我们发布了没有问题的代码到产品中，程序会是正确的，并且我们能安全的移除检查。

下面是我建议的不要用断言的场景：

* 不要用它测试用户提供的数据
* 不要用断言来检查你觉得在你的程序的常规使用时会出错的地方。断言是用来检查非常罕见的问题。你的用户不应该看到任何断言错误，如果他们看到了，这是一个bug，修复它。
* 有的情况下，不用断言是因为它比精确的检查要短，它不应该是懒码农的偷懒方式。
* 不要用它来检查对公共库的输入参数，因为它不能控制调用者，所以不能保证调用者会不会打破双方的约定。
* 不要为你觉得可以恢复的错误用断言。换句话说，不用改在产品代码里捕捉到断言错误。
* 不要用太多断言以至于让代码很晦涩。