---
layout:     post
title:      "sys.argv[]用法"
subtitle:   "sys.argv[]"
date:       2017-10-19
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Python
---

在学python的过程中，一直弄不明白sys.argv[]的意思，虽知道是表示命令行参数，但还是有些稀里糊涂的感觉。

今天又好好学习了一把，总算是大彻大悟了。

Sys.argv[]是用来获取命令行参数的，sys.argv[0]表示代码本身文件路径，所以参数从1开始，以下两个例子说明：

1、使用sys.argv[]的一简单实例，

```
1. import sys,os  
2. os.system(sys.argv[1])  
```

这个例子os.system接收命令行参数，运行参数指令，保存为sample1.py，命令行带参数运行sample1.py notepad，将打开记事本程序。

2、这个例子是简明python教程上的，明白它之后你就明白sys.argv[]了。

```
1. import sys  
2. def readfile(filename):  #从文件中读出文件内容  
3.     '''''Print a file to the standard output.'''  
4.     f = file(filename)  
5.     while True:  
6.         line = f.readline()  
7.         if len(line) == 0:  
8.             break  
9.         print line, # notice comma  分别输出每行内容  
10.     f.close()  
11. # Script starts from here  
12. if len(sys.argv) < 2:  
13.     print 'No action specified.'  
14.     sys.exit()  
15. if sys.argv[1].startswith('--'):  
16.     option = sys.argv[1][2:]  
17.     # fetch sys.argv[1] but without the first two characters  
18.     if option == 'version':  #当命令行参数为-- version，显示版本号  
19.         print 'Version 1.2'  
20.     elif option == 'help':  #当命令行参数为--help时，显示相关帮助内容  
21.         print '''''/
22. This program prints files to the standard output.
23. Any number of files can be specified.
24. Options include:
25.   --version : Prints the version number
26.   --help    : Display this help'''  
27.     else:  
28.         print 'Unknown option.'  
29.     sys.exit()  
30. else:  
31.     for filename in sys.argv[1:]: #当参数为文件名时，传入readfile，读出其内容  
32.         readfile(filename)  
```

保存程序为sample.py.我们验证一下：

1. 命令行带参数运行：sample.py –version  输出结果为：version 1.2

2. 命令行带参数运行：sample.py –help  输出结果为：This program prints files……

3. 在与sample.py同一目录下，新建a.txt的记事本文件，内容为：test argv；命令行带参数运行：sample.py a.txt，输出结果为a.txt文件内容：test argv，这里也可以多带几个参数，程序会先后输出参数文件内容。
