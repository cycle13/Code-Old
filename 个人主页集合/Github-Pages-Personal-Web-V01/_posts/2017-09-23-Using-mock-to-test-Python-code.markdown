---
layout:     post
title:      "用Python中的mock库对Python代码进行模拟测试"
subtitle:   "Using mock to test Python code"
date:       2017-09-23
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

# 如何不靠耐心测试

通常，我们编写的软件会直接与那些我们称之为“肮脏的”服务交互。通俗地说，服务对我们的应用来说是至关重要的，它们之间的交互是我们设计好的，但这会带来我们不希望的副作用——就是那些在我们自己测试的时候不希望的功能。

比如，可能我们正在写一个社交软件并且想测试一下“发布到Facebook的功能”，但是我们不希望每次运行测试集的时候都发布到Facebook上。


Python的unittest库中有一个子包叫unittest.mock——或者你把它声明成一个依赖，简化为mock——这个模块提供了非常强大并且有用的方法，通过它们可以模拟或者屏敝掉这些不受我们希望的方面。

注意：mock是最近收录在Python 3.3标准库中的；之前发布的版本必须通过 PyPI下载Mock库。

# 恐惧系统调用

再举一个例子，考虑系统调用，我们将在余下的文章中讨论它们。不难发现，这些都可以考虑使用模拟：无论你是想写一个脚本弹出一个CD驱动，或者是一个web服务用来删除/tmp目录下的缓存文件，或者是一个socket服务来绑定一个TCP端口，这些调用都是在你单元测试的时候是不被希望的方面。

作为一个开发人员，你更关心你的库是不是成功的调用了系统函数来弹出CD，而不是体验每次测试的时候CD托盘都打开。

作为一个开发人员，你更关心你的库是不是成功调用了系统函数来弹出CD（带着正确的参数等）。而不是体验每次测试的时候CD托盘都打开（或者更糟，很多次，当一个单元测试运行的时候，很多测试点都涉及到了弹出代码）。

同样地，保持你的单元测试效率和性能意味着要还要保留一些自动化测试之外的“缓慢代码”，比如文件系统和网络的访问。

对于我们的第一个例子，我们要重构一个从原始到使用mock的一个标准Python测试用例。我们将会证明如何用mock写一个测试用例使我们的测试更智能、更快，并且能暴露更多关于我们的软件工作的问题。

# 一个简单的删除功能

有时，我们需要从文件系统中删除文件，因此，我们可以写这样的一个函数在Python中，这个函数将使它更容易成为我们的脚本去完成这件事情。

```
#!/usr/bin/env python
# -*- coding: utf-8 -*-
 
import os
 
def rm(filename):
    os.remove(filename)
```

很明显，在这个时间点上，我们的rm方法不提供比基本os.remove方法更多的功能，但我们的代码将会有所改进，允许我们在这里添加更多的功能。

让我们写一个传统的测试用例，即，不用模拟测试：

```
#!/usr/bin/env python
# -*- coding: utf-8 -*-
 
from mymodule import rm
 
import os.path
import tempfile
import unittest
 
class RmTestCase(unittest.TestCase):
 
    tmpfilepath = os.path.join(tempfile.gettempdir(), "tmp-testfile")
 
    def setUp(self):
        with open(self.tmpfilepath, "wb") as f:
            f.write("Delete me!")
 
    def test_rm(self):
        # remove the file
        rm(self.tmpfilepath)
        # test that it was actually removed
        self.assertFalse(os.path.isfile(self.tmpfilepath), "Failed to remove the file.")
```

我们的测试用例是相当简单的，但当它每次运行时，一个临时文件被创建然后被删除。此外，我们没有办法去测试我们的rm方法是否传递参数到os.remove中。我们可以假设它是基于上面的测试，但仍有许多需要被证实。

# 重构与模拟测试

让我们使用mock重构我们的测试用例：

```
#!/usr/bin/env python
# -*- coding: utf-8 -*-
 
from mymodule import rm
 
import mock
import unittest
 
class RmTestCase(unittest.TestCase):
 
    @mock.patch('mymodule.os')
    def test_rm(self, mock_os):
        rm("any path")
        # test that rm called os.remove with the right parameters
        mock_os.remove.assert_called_with("any path")

```

对于这些重构，我们已经从根本上改变了该测试的运行方式。现在，我们有一个内部的对象，让我们可以使用另一个功能验证。

# 潜在的陷阱

第一件要注意的事情就是，我们使用的mock.patch方法的装饰位于mymodule.os模拟对象，并注入到我们测试案例的模拟方法。是模拟os更有意义，还是它在mymodule.os的参考更有意义？

当然，当Python出现在进口和管理模块时，用法是非常的灵活。在运行时，该mymodule模块有自己的os操作系统——被引入到自己的范围内的模块。因此，如果我们模拟os系统，我们不会看到模拟测试在mymodule模块的影响。

这句话需要深刻的记住：

> 模拟测试一个项目，只需要了解它用在哪里，而不是它从哪里来.

如果你需要为myproject.app.MyElaborateClass模拟tempfile模型，你可能需要去模拟myproject.app.tempfile的每个模块来保持自己的进口。

这就是用陷阱的方式来模拟测试。

# 向'rm'中加入验证

之前定义的 rm 方法相当的简单 . 在盲目的删除之前，我们会拿它来验证一个路径是否存在，并验证其是否是一个文件. 让我们重构 rm 使其变得更加聪明:

```
#!/usr/bin/env python
# -*- coding: utf-8 -*-
 
import os
import os.path
 
def rm(filename):
    if os.path.isfile(filename):
        os.remove(filename)
```

很好. 现在，让我们调整我们的测试用例来保持测试的覆盖程度。

```
#!/usr/bin/env python
# -*- coding: utf-8 -*-
 
from mymodule import rm
 
import mock
import unittest
 
class RmTestCase(unittest.TestCase):
 
    @mock.patch('mymodule.os.path')
    @mock.patch('mymodule.os')
    def test_rm(self, mock_os, mock_path):
        # set up the mock
        mock_path.isfile.return_value = False
 
        rm("any path")
 
        # test that the remove call was NOT called.
        self.assertFalse(mock_os.remove.called, "Failed to not remove the file if not present.")
 
        # make the file 'exist'
        mock_path.isfile.return_value = True
 
        rm("any path")
 
        mock_os.remove.assert_called_with("any path")

```

我们的测试范例完全变化了. 现在我们可以核实并验证方法的内部功能是否有任何副作用.

# 将删除功能作为服务

到目前为止，我们只是对函数功能提供模拟测试，并没对需要传递参数的对象和实例的方法进行模拟测试。接下来我们将介绍如何对对象的方法进行模拟测试。

首先，我们先将rm方法重构成一个服务类。实际上将这样一个简单的函数转换成一个对象并不需要做太多的调整，但它能够帮助我们了解mock的关键概念。下面是重构的代码:

```
#!/usr/bin/env python
# -*- coding: utf-8 -*-
 
import os
import os.path
 
class RemovalService(object):
    """A service for removing objects from the filesystem."""
 
    def rm(filename):
        if os.path.isfile(filename):
            os.remove(filename)

```

你可以发现我们的测试用例实际上没有做太多的改变：

```
#!/usr/bin/env python
# -*- coding: utf-8 -*-
 
from mymodule import RemovalService
 
import mock
import unittest
 
class RemovalServiceTestCase(unittest.TestCase):
 
    @mock.patch('mymodule.os.path')
    @mock.patch('mymodule.os')
    def test_rm(self, mock_os, mock_path):
        # instantiate our service
        reference = RemovalService()
 
        # set up the mock
        mock_path.isfile.return_value = False
 
        reference.rm("any path")
 
        # test that the remove call was NOT called.
        self.assertFalse(mock_os.remove.called, "Failed to not remove the file if not present.")
 
        # make the file 'exist'
        mock_path.isfile.return_value = True
 
        reference.rm("any path")
 
        mock_os.remove.assert_called_with("any path")
```

很好，RemovalService如同我们计划的一样工作。接下来让我们创建另一个以该对象为依赖项的服务:

```
#!/usr/bin/env python
# -*- coding: utf-8 -*-
 
import os
import os.path
 
class RemovalService(object):
    """A service for removing objects from the filesystem."""
 
    def rm(self, filename):
        if os.path.isfile(filename):
            os.remove(filename)
 
class UploadService(object):
 
    def __init__(self, removal_service):
        self.removal_service = removal_service
 
    def upload_complete(self, filename):
        self.removal_service.rm(filename)
```

到目前为止，我们的测试已经覆盖了RemovalService， 我们不会对我们测试用例中UploadService的内部函数rm进行验证。相反，我们将调用UploadService的RemovalService.rm方法来进行简单的测试（为了不产生其他副作用），我们通过之前的测试用例可以知道它可以正确地工作。

有两种方法可以实现以上需求:

1. 模拟RemovalService.rm方法本身。
2. 在UploadService类的构造函数中提供一个模拟实例。

因为这两种方法都是单元测试中非常重要的方法，所以我们将同时对这两种方法进行回顾。

## 选项1: 模拟实例的方法

该模拟库有一个特殊的方法用来装饰模拟对象实例的方法和参数。@mock.patch.object 进行装饰：

```
#!/usr/bin/env python
# -*- coding: utf-8 -*-
 
from mymodule import RemovalService, UploadService
 
import mock
import unittest
 
class RemovalServiceTestCase(unittest.TestCase):
 
    @mock.patch('mymodule.os.path')
    @mock.patch('mymodule.os')
    def test_rm(self, mock_os, mock_path):
        # instantiate our service
        reference = RemovalService()
 
        # set up the mock
        mock_path.isfile.return_value = False
 
        reference.rm("any path")
 
        # test that the remove call was NOT called.
        self.assertFalse(mock_os.remove.called, "Failed to not remove the file if not present.")
 
        # make the file 'exist'
        mock_path.isfile.return_value = True
 
        reference.rm("any path")
 
        mock_os.remove.assert_called_with("any path")
 
class UploadServiceTestCase(unittest.TestCase):
 
    @mock.patch.object(RemovalService, 'rm')
    def test_upload_complete(self, mock_rm):
        # build our dependencies
        removal_service = RemovalService()
        reference = UploadService(removal_service)
 
        # call upload_complete, which should, in turn, call `rm`:
        reference.upload_complete("my uploaded file")
 
        # check that it called the rm method of any RemovalService
        mock_rm.assert_called_with("my uploaded file")
 
        # check that it called the rm method of _our_ removal_service
        removal_service.rm.assert_called_with("my uploaded file")

```

太棒了！我们验证了上传服务成功调用了实例的rm方法。你是不是注意到这当中有意思的地方了？这种修补机制实际上取代了我们的测试方法的删除服务实例的rm方法。这意味着，我们实际上可以检查该实例本身。如果你想了解更多，可以试着在模拟测试的代码中下断点来更好的认识这种修补机制是如何工作的。

### 陷阱：装饰的顺序

当使用多个装饰方法来装饰测试方法的时候，装饰的顺序很重要，但很容易混乱。基本上，当装饰方法呗映射到带参数的测试方法中时，装饰方法的工作顺序是反向的。比如下面这个例子：

```
@mock.patch('mymodule.sys')
@mock.patch('mymodule.os')
@mock.patch('mymodule.os.path')
def test_something(self, mock_os_path, mock_os, mock_sys):
    pass
``` 

注意到了吗，我们的装饰方法的参数是反向匹配的？ 这是有部分原因是因为Python的工作方式。下面是使用多个装饰方法的时候，实际的代码执行顺序：

```
patch_sys(patch_os(patch_os_path(test_something)))
```

由于这个关于sys的补丁在最外层，因此会在最后被执行，使得它成为实际测试方法的最后一个参数。请特别注意这一点，并且在做测试使用调试器来保证正确的参数按照正确的顺序被注入。

## 选项2: 创建模拟测试接口

我们可以在UploadService的构造函数中提供一个模拟测试实例，而不是模拟创建具体的模拟测试方法。 我推荐使用选项1的方法，因为它更精确，但在多数情况下，选项2是必要的并且更加有效。让我们再次重构我们的测试实例：

```
#!/usr/bin/env python
# -*- coding: utf-8 -*-
 
from mymodule import RemovalService, UploadService
 
import mock
import unittest
 
class RemovalServiceTestCase(unittest.TestCase):
 
    @mock.patch('mymodule.os.path')
    @mock.patch('mymodule.os')
    def test_rm(self, mock_os, mock_path):
        # instantiate our service
        reference = RemovalService()
 
        # set up the mock
        mock_path.isfile.return_value = False
 
        reference.rm("any path")
 
        # test that the remove call was NOT called.
        self.assertFalse(mock_os.remove.called, "Failed to not remove the file if not present.")
 
        # make the file 'exist'
        mock_path.isfile.return_value = True
 
        reference.rm("any path")
 
        mock_os.remove.assert_called_with("any path")
 
class UploadServiceTestCase(unittest.TestCase):
 
    def test_upload_complete(self, mock_rm):
        # build our dependencies
        mock_removal_service = mock.create_autospec(RemovalService)
        reference = UploadService(mock_removal_service)
 
        # call upload_complete, which should, in turn, call `rm`:
        reference.upload_complete("my uploaded file")
 
        # test that it called the rm method
        mock_removal_service.rm.assert_called_with("my uploaded file")

```

在这个例子中，我们甚至不需要补充任何功能，只需创建一个带auto-spec方法的RemovalService类，然后将该实例注入到UploadService中对方法验证。

mock.create_autospec为类提供了一个同等功能实例。这意味着，实际上来说，在使用返回的实例进行交互的时候，如果使用了非法的方法将会引发异常。更具体地说，如果一个方法被调用时的参数数目不正确，将引发一个异常。这对于重构来说是非常重要。当一个库发生变化的时候，中断测试正是所期望的。如果不使用auto-spec，即使底层的实现已经破坏，我们的测试仍然会通过。

### 陷阱：mock.Mock和mock.MagicMock类

mock库包含两个重要的类mock.Mock和mock.MagicMock，大多数内部函数都是建立在这两个类之上的。在选择使用mock.Mock实例，mock.MagicMock实例或auto-spec方法的时候,通常倾向于选择使用 auto-spec方法，因为它能够对未来的变化保持测试的合理性。这是因为mock.Mock和mock.MagicMock会无视底层的API，接受所有的方法调用和参数赋值。比如下面这个用例：

```
class Target(object):
    def apply(value):
        return value
 
def method(target, value):
    return target.apply(value)
```

我们像下面这样使用mock.Mock实例来做测试：

```
class MethodTestCase(unittest.TestCase):
 
    def test_method(self):
        target = mock.Mock()
 
        method(target, "value")
 
        target.apply.assert_called_with("value")
```

这个逻辑看似合理，但如果我们修改Target.apply方法接受更多参数：

```
class Target(object):
    def apply(value, are_you_sure):
        if are_you_sure:
            return value
        else:
            return None
```

重新运行你的测试，然后你会发现它仍然能够通过。这是因为它不是针对你的API创建的。这就是为什么你总是应该使用create_autospec方法，并且在使用@patch和@patch.object装饰方法时使用autospec参数。

# 真实世界的例子: 模仿一次 Facebook API 调用

在结束之际，让我写一个更加实用的真实世界的例子, 这在我们的介绍部分曾今提到过: 向Facebook发送一个消息. 我们会写一个漂亮的封装类，和一个产生回应的测试用例

```
import facebook
 
class SimpleFacebook(object):
 
    def __init__(self, oauth_token):
        self.graph = facebook.GraphAPI(oauth_token)
 
    def post_message(self, message):
        """Posts a message to the Facebook wall."""
        self.graph.put_object("me", "feed", message=message)
```

下面是我们的测试用例, 它检查到我发送了信息，但并没有实际的发送出这条信息（到Facebook上）:

```
import facebook
import simple_facebook
import mock
import unittest
 
class SimpleFacebookTestCase(unittest.TestCase):
 
    @mock.patch.object(facebook.GraphAPI, 'put_object', autospec=True)
    def test_post_message(self, mock_put_object):
        sf = simple_facebook.SimpleFacebook("fake oauth token")
        sf.post_message("Hello World!")
 
        # verify
        mock_put_object.assert_called_with(message="Hello World!")
```

就我们目前所看到的，在Python中用 mock 开始编写更加聪明的测试是真的很简单的.

# 总结

Python的 mock 库, 使用起来是有点迷惑, 是单元测试的游戏规则变革者. 我们通过开始在单元测试中使用 mock ，展示了一些通常的使用场景, 希望这篇文章能帮助 Python 克服一开始的障碍，写出优秀的，能经得起测试的代码.
