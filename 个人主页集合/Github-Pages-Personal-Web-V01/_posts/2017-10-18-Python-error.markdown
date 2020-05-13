---
layout:     post
title:      "Python异常编程技巧"
subtitle:   "Python exception programming skills"
date:       2017-10-18
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

编程中经常会需要使用到异常处理的情况，在阅读了一些资料后，整理了关于异常处理的一些小技巧记录如下。

# 如何自定义异常

## 定义异常类

在实际编程中，有时会发现Python提供的内建异常的不够用，我们需要在特殊业务场景下的异常。这时就需要我们来定义自己的异常。按照Python约定俗成的习惯，用户定义的异常一般都是继承于Exception类，由它开始拓展。后面我们可以看到这样做在捕获异常的时候会带来很大的便利。

```
>>> class MyError(Exception):
        pass

>>> raise MyError(u"something error")
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
__main__.MyError: something error
```

# API异常相关的技巧

API的异常分为定义异常与调用API时如何捕获异常两个部分，这二者相辅相成。

## 定义API异常的技巧

在自己编写API的时候，应该定义Root Exception——API中的根异常，其它异常都继承于它。这样的做法有两个好处：

1. API代码层次更清晰
2. API与调用程序代码隔离

假设存在如下场景：需要做一个链接数据库服务的模块。提供一个connect函数用于链接。那么，在链接的过程中，就会发生以下几种情况：

* socket连接超时
* socket拒绝连接

针对以上的情况，我们在模块中定义几个异常：

```
# database.py
class Error(Exception):
    """Root exception for all exceptions raised by this module."""

class SocketTimeError(Error):
    pass

class SocketRefuseError(Error):
    pass

def connect():
    pass    
```

# 调用API时异常捕获的技巧

这样在调用API的时候就可以这样使用：

```
try:
    connect()
except SocketTimeError as err:
    log.error(err)
except SocketRefuseError as err:
    log.error(err)
except Error as err:
    log.error("API Unexpected error:%s" % err)
except Exception:
    log.error("API bug cause exception.")    
```

这样精确定义多个异常，使得代码层次清晰，增强了可读性。值得注意的是：在代码的最后还捕获了Error以及Exception两个异常，这两个操作分别对应于可拓展性与健壮性的目的。

**捕获Root Exception以提高可拓展性：**

我们知道，在实际链接数据库时，还可能会出现用户没有登陆权限等问题。所以，我们需要在下一个版本中加入PermissionDeny这个异常。但是，旧的调用代码已经写好了，如果忘记修改的话，这个异常可能就会无法被处理，进而使得调用的程序奔溃。处于这样的考虑，我们在调用API的时候，就应该再捕获API的Root Exception，即使之后新加入了其它的异常，在这一个except中也能被捕获而不影响调用程序。使得API模块的可拓展性得到了提高。

**捕获Exception以提高健壮性：**

在调用API的时候，难免可能出现API内部存在bug的情况。这个时候如果捕获了Exception的话，就算API内部因为bug发生了异常，也不会影响到调用程序的正常运行。

从这两点中可以看出，要达到这种效果，其实都要依赖于常规异常继承于Exception类这个规矩。这样的架构划分所带来的好处是显而易见的。

# 与异常相关的编程艺术

## 异常代替返回状态码

我们经常需要编写一些工具类的函数，往往在这些函数的处理流程中，会产生很多的状态；而这些状态也是调用者需要得到的信息。很多时候，会用一些具有意义的返回值来表示函数处理的状态。

比如：

```
def write(content):
    if isinstance(content, basestring):
        f_handler = open("file.txt", 'w')
        try:
            f_handler.write(context)
            except Exception:
                return -2    # write file fail
        else:
            return 0    # write file succcess
        finally:
            f_hanlder.close()
    else:
        return -1    # arg type error
```

调用代码：

```
result = write()
if result == -1:
    log.error(u"type error")
elif result = -2:
    log.error(u"write error")
else:
    log.info("ok")    
```

这种状态码的方式使用起来特别的不方便，调用者还需要去理解每个状态码的意义，带来其它的学习成本；而且用if-else结构也不易于后期的程序拓展。所以，我们可以使用触发异常来代替返回状态码，每个异常名其实就包含了状态的意义在内(命名的艺术)，使用起来也更好理解。

使用异常的方式：

```
class Error(Exception):
    pass

class OpenFileError(Error):
    pass

class WriteContentError(Error):
    pass    

def write(content):
    if isinstance(content, basestring):
        f_handler = open("file.txt", 'w')
        try:
            f_handler.write(context)
            except Exception:
                raise WriteContentError
        finally:
            f_hanlder.close()
    else:
        raise OpenFileError
```

调用代码：

```
try:
    write()
except OpenFileError as e:
    log.error(e)
except WriteContentError as e:
    log.error(e)
except Error:
    log.error("API Error")
except Exception
    log.error("API Bug")    
else:
    log.info("ok")
```

结合上面一点提到的使用API时的异常捕获，使得调用代码变得更佳灵活。

## 异常处理与流程控制

错误处理很重要，但如果它搞乱了代码逻辑，就是错误的做法

将异常处理与正常流程控制混为一谈时，代码是十分丑陋的。我们应该将二者分离，最好的做法就是将异常代码块抽离到另外的函数中。

```
try:
    action_a()
    action_b()
    action_c()
except ActionException as e:
    log.error(e)
else:
    action_d()    
```

将异常处理分离：

```
def action_executor():
    action_a()
    action_b()
    action_c()

def action():
    try:
        action_executor()
    except ActionException as e:
        log.error(e)

action()
action_d()
```
