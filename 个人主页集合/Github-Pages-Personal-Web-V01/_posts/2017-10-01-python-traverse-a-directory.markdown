---
layout:     post
title:      "Python 遍历一个目录"
subtitle:   "Python traverse a directory"
date:       2017-10-01
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---


有时候，你会发现你不得不写一段代码来遍历一个目录。以我经验来说，它们可能是一个一次性脚本或者清理脚本。不管怎么说，Pyhton提供了一个非常有用遍历目录结构的方法，它叫os.walk。我通常用它来遍历一个目录集或者一个子目录集，进而从目录中删除一些旧文件或者不需要的垃圾文件。现在就让我们学习一下如何使用Python来遍历目录吧！

# 使用os.walk

只需多加练习，你就知道如何正确使用它了。这里有个示例，会打印出你所指定路径下所有文件名：

```
import os
 
def pywalker(path):
    for root, dirs, files in os.walk(path):
        for file_ in files:
            print( os.path.join(root, file_) )
 
if __name__ == '__main__':
    pywalker('/path/to/some/folder')
```

通过加入root和file_变量，你最终打印出完整文件路径。如果你想要知道文件的创建日期，你可以使用os.stat. 举个例子，我曾经使用这个来创建一个清理脚本。

如果你想列出指定路径中所有文件夹和文件的话，你可以考虑使用os.listdir。通常，我必须找到最底层的子文件夹，所以listdir不是一个很好的选择。这个时候，我会使用os.walk来代替它。

# 直接使用os.scandir()

最新的Python 3.5版本中，新添加了os.scandir()方法，它是一个目录迭代方法。你可以在PEP 471中找到关于它的一些内容。在Python 3.5中，os.walk是使用os.scandir来实现的，根据Python 3.5宣称，它在POSIX系统中，执行速度提高了3到5倍；在Windows系统中，执行速度提高了7到20倍。

让我们用os。scandir来实现最简单的示例。

```
import os
 
folders = []
files = []
 
for entry in os.scandir('/'):
    if entry.is_dir():
        folders.append(entry.path)
    elif entry.is_file():
        files.append(entry.path)
 
print('Folders:')
print(folders)
```

scandir方法返回了一个DirEntry迭代器对象，它非常轻巧方便，并且能告诉你迭代文件的路径。之前案例中，我们检查了entry是一个文件或者是一个文件夹，与此同时，我们添加它的路径到列表中。你也可以通过DirEntry的stat方法来获取它的stat对象，这个方法非常棒！

# 总结

你现在知道如何使用Python对目录文件进行遍历了。如果你想更有效的进行遍历，你可以使用Python 3.5以后的版本，它已内置了一个模块os.scandir。你可以从PyPI上获取这个scandir包。