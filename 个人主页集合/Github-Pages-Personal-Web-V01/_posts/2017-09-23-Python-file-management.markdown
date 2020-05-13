---
layout:     post
title:      "Python文件管理"
subtitle:   "Python file management"
date:       2017-09-23
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

# Python中的文件管理

文件管理是很多应用程序的基本功能和重要组成部分。Python可以使文件管理极其简单，特别是和其它语言相对比。

## 介绍

你玩过的游戏使用文件来保存存档；你下的订单保存在文件中；很明显，你早上写的报告也保存在文件中。

几乎以任何语言编写的众多应用程序中，文件管理是很重要的一部分。Python当然也不例外。在这篇文章中，我们将探究如何使用一些模块来操作文件。我们会完成读文件，写文件，增加文件内容的操作，写文件，增加文件内容的操作，还有一些另类的用法。OK，让我们开始吧。

## 读写文件

最基本的文件操作当然就是在文件中读写数据。这也是很容易掌握的。现在打开一个文件以进行写操作：

```
fileHandle = open ( 'test.txt', 'w' )
```

'w'是指文件将被写入数据，语句的其它部分很好理解。下一步就是将数据写入文件：

```
fileHandle.write ( 'This is a test./nReally, it is.' )
```

这个语句将“This is a test.”写入文件的第一行，“Really, it is.”写入文件的第二行。最后，我们需要做清理工作，并且关闭文件：

```
fileHandle.close()
```

正如你所见，在Python的面向对象机制下，这确实非常简单。需要注意的是，当你再次使用“w”方式在文件中写数据，所有原来的内容都会被删除。如果想保留原来的内容，可以使用“a”方式在文件中结尾附加数据：

```
fileHandle = open ( 'test.txt', 'a' )
fileHandle.write ( '/n/nBottom line.' )
fileHandle.close()
```

然后，我们读取test.txt，并将内容显示出来：

```
fileHandle = open ( 'test.txt' )
print fileHandle.read()
fileHandle.close()
```

以上语句将读取整个文件并显示其中的数据。我们也可以读取文件中的一行：

```
fileHandle = open ( 'test.txt' )
print fileHandle.readline() # "This is a test."
fileHandle.close()
```

同时，也可以将文件内容保存到一个list中：

```
fileHandle = open ( 'test.txt' )
fileList = fileHandle.readlines()<div></div>
for fileLine in fileList:
print '>>', fileLine
fileHandle.close()
```

Python在读取一个文件时，会记住其在文件中的位置，如下所示：

```
fileHandle = open ( 'test.txt' )
garbage = fileHandle.readline()
fileHandle.readline() # "Really, it is."fileHandle.close()
```

可以看到，只有第二行显示出来。然而，我们可以让Python从头开始读来解决这个问题：

```
fileHandle = open ( 'test.txt' )
garbage = fileHandle.readline()
fileHandle.seek ( 0 )
print fileHandle.readline() # "This is a test."
fileHandle.close()
```

在上面这个例子中，我们让Python从文件第一个字节开始读取数据。所以，第一行文字显示了出来。当然，我们也可以获取Python在文件中的位置：

```
fileHandle = open ( 'test.txt' )
print fileHandle.readline() # "This is a test."
print fileHandle.tell() # "17"
print fileHandle.readline() # "Really, it is."
```

或者在文件中一次读取几个字节的内容：

```
fileHandle = open ( 'test.txt' )
print fileHandle.read ( 1 ) # "T"
fileHandle.seek ( 4 )
print FileHandle.read ( 1 ) # " "(原文有错)
```

在Windows和Macintosh环境下，有时可能需要以二进制方式读写文件，比如图片和可执行文件。此时，只要在打开文件的方式参数中增加一个“b”即可：

```
fileHandle = open ( 'testBinary.txt', 'wb' )
fileHandle.write ( 'There is no spoon.' )
fileHandle.close()
```

```
fileHandle = open ( 'testBinary.txt', 'rb' )
print fileHandle.read()
fileHandle.close()
```

# 从现有文件中获取信息

使用Python中的模块，可以从现有文件中获取信息。使用“os”模块和“stat”模块可以获取文件的基本信息：

```
import os
import stat
import time<div></div>
fileStats = os.stat ( 'test.txt' )
fileInfo = {
'Size' : fileStats [ stat.ST_SIZE ],
'LastModified' : time.ctime ( fileStats [ stat.ST_MTIME ] ),
'LastAccessed' : time.ctime ( fileStats [ stat.ST_ATIME ] ),
'CreationTime' : time.ctime ( fileStats [ stat.ST_CTIME ] ),
'Mode' : fileStats [ stat.ST_MODE ]
}

for infoField, infoValue in fileInfo:
	print infoField, ':' + infoValue
	if stat.S_ISDIR ( fileStats [ stat.ST_MODE ] ):
		print 'Directory. '
	else:
		print 'Non-directory.'
```

上面这个例子创建了一个包含文件基本信息的dictionary。然后显示了相关信息，并且告诉我们打开的是否为目录。我们也可以试一下打开的是否是其它几种类型：

```
import os
import stat
fileStats = os.stat ( 'test.txt' )
fileMode = fileStats [ stat.ST_MODE ]
if stat.S_ISREG ( fileStats [ stat.ST_MODE ] ):
	print 'Regular file.'
elif stat.S_ISDIR ( fileStats [ stat.ST_MODE ] ):
	print 'Directory.'
elif stat.S_ISLNK ( fileStats [ stat.ST_MODE ] ):
	print 'Shortcut.'
elif stat.S_ISSOCK ( fileStats [ stat.ST_MODE ] ):
	print 'Socket.'
elif stat.S_ISFIFO ( fileStats [ stat.ST_MODE ] ):
	print 'Named pipe.'
elif stat.S_ISBLK ( fileStats [ stat.ST_MODE ] ):
	print 'Block special device.'
elif stat.S_ISCHR ( fileStats [ stat.ST_MODE ] ):
	print 'Character special device.'
```

另外，我们可以使用“os.path”来获取基本信息：

```
import os.path
fileStats = 'test.txt'
if os.path.isdir ( fileStats ):
	print 'Directory.'
elif os.path.isfile ( fileStats ):
	print 'File.'
elif os.path.islink ( fileStats ):
	print 'Shortcut.'
elif os.path.ismount ( fileStats ):
	print 'Mount point.'
```

# 目录

和普通文件一样，关于目录的操作也很容易掌握。首先，列出一个目录的内容：

```
import os
for fileName in os.listdir ( '/' ):
	print fileName
```

正如你所见，这很简单，用三行代码就可以完成。

创建目录也很简单：

```
import os
os.mkdir ( 'testDirectory' )
```

删除刚才创建的目录：

```
import os
os.rmdir ( 'testDirectory )
```

嗯，可以创建多级目录：

```
import os
os.makedirs ( 'I/will/show/you/how/deep/the/rabbit/hole/goes' )
```

如果没有在创建的文件夹中添加任何东西，就可以一次性将它们全部删除（即，删除所列的所有空文件夹）：

```
import os
os.removedirs ( 'I/will/show/you/how/deep/the/rabbit/hole/goes' )
```

当需要对一个特定的文件类型进行操作时，我们可以选择“fnmatch”模块。以下是显示“.txt”文件的内容和“.exe”文件的文件名：

```
import fnmatch
import os
for fileName in os.listdir ( '/' ):
	if fnmatch.fnmath ( fileName, '*.txt' ):
		print open ( fileName ).read()
	elif fnmatch.fnmatch ( fileName, '*.exe' ):
		print fileName
```

“*”字符可以表示任意长度的字符。如果要匹配一个字符，则使用“?”符号：


```
import fnmatch
import os
for fileName in os.listdir ( '/' ):
	if fnmatch.fnmatch ( fileName, '?.txt' ):
		print 'Text file.'
```

“fnmatch”模块支持正则表达式：

```
import fnmatch
import os
import re
filePattern = fnmatch.translate ( '*.txt' )
for fileName in os.listdir ( '/' ):
	if re.match ( filePattern, fileName ):
		print 'Text file.'
```

若只需要匹配一种类型的文件，更好的办法是使用“glob”模块。该模块的格式和“fnmatch”相似：

```
import glob
for fileName in glob.glob ( '*.txt' ):
	print 'Text file.'
```

使用一定范围的字符来匹配同样可行，就像在正则表达式中使用一样。假设你想要显示扩展名前只有一位数字的文件的文件名：

```
import glob
for fileName in glob.glob ( '[0-9].txt' ):
	print filename
```

“glob”模块利用“fnmatch”模块来实现。

# 数据编组

使用前一节中介绍的模块，可以实现在文件中对字符串的读写。

然而，有的时候，你可能需要传递其它类型的数据，如list、tuple、dictionary和其它对象。在Python中，你可以使用Pickling来完成。你可以使用Python标准库中的“pickle”模块完成数据编组。

下面，我们来编组一个包含字符串和数字的list：

```
import pickle
fileHandle = open ( 'pickleFile.txt', 'w' )
testList = [ 'This', 2, 'is', 1, 'a', 0, 'test.' ]
pickle.dump ( testList, fileHandle )
fileHandle.close()
```

拆分编组同样不难：

```
import pickle
fileHandle = open ( 'pickleFile.txt' )
testList = pickle.load ( fileHandle )
fileHandle.close()
```

现在试试存储更加复杂的数据：

```
import pickle
fileHandle = open ( 'pickleFile.txt', 'w' )
testList = [ 123, { 'Calories' : 190 }, 'Mr. Anderson', [ 1, 2, 7 ] ]
pickle.dump ( testList, fileHandle )
fileHandle.close()
```

```
import pickle
fileHandle = open ( 'pickleFile.txt' )
testList = pickle.load ( fileHandle )
fileHandle.close()
```

如上所述，使用Python的“pickle”模块编组确实很简单。众多对象可以通过它来存储到文件中。如果可以的话，“cPickle”同样胜任这个工作。它和“pickle”模块一样，但是速度更快：

```
import cPickle
fileHandle = open ( 'pickleFile.txt', 'w' )
cPickle.dump ( 1776, fileHandle )
fileHandle.close()
```

# 创建“虚拟”文件

你用到的许多模块包含需要文件对象作为参数的方法。但是，有时创建并使用一个真实的文件并让人感到有些麻烦。所幸的是，在Python中，你可以使用“StringIO”模块来创建文件并将其保存在内存中：

```
import StringIO
fileHandle = StringIO.StringIO ( "Let freedom ring" )
print fileHandle.read() # "Let freedom ring."
fileHandle.close()
```

cStringIO”模块同样有效。它的使用方法和“StringIO”一样，但就像“cPickle”之于“pickle”，它速度更快：

```
import cStringIO
fileHandle = cStringIO.cStringIO ( "To Kill a Mockingbird" )
print fileHandle.read() # "To Kill a Mockingbid"
fileHandle.close()
```

# 结论

文件管理，是众多编程语言的程序员在编写应用程序是经常遇到的问题。幸好，和其它语言相比，Python使其出乎意料地容易。Python的标准库中提供了许多相关的模块帮助程序员解决这方面的问题，而它的面向对象的机制也简化了操作。

