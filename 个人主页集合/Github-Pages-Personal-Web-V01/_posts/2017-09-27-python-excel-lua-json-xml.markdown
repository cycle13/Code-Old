---
layout:     post
title:      "Python导出Excel为Lua/Json/Xml实例教程"
subtitle:   "Export Excel to Lua/Json/Xml using Python"
date:       2017-09-27
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

# 一、什么是xlrd

xlrd是python语言中读取excel的扩展工具，可以实现指定表单、指定单元格的读取。

# 二、安装xlrd

* 官网安装：到python官网http://pypi.python.org/pypi/xlrd下载模块安装，前提是已经安装了python 环境。
* MacOS安装： $ sudo pip install xlrd

# 三、基本用法

## 1.读取excel文件

```
workbook = xlrd.open_workbook("file.xls")
```

## 2.获取excel中表单数量

```
workbook.nsheets
```

## 3.获取excel中的一个表单

```
workbook.sheets()[i]
workbook.sheet_by_index(i)
workbook.sheet_by_name(u"Sheet1")
```

## 4.获取行数

```
sheet.nrows
```

## 5.获取列数

```
sheet.ncols
```

## 6.获取整行数据

```
sheet.row(i)
```

## 7.获取整列数据

```
sheet.col(i)
```

## 8.获取单元格数据

```
sheet.cell(i, j).value
```

知道这么多，已经足够对Excel进行基本的操作和读取了。

# 四、简单上手 - 遍历表中数据

1.先来创建一个Excel表名为Configs.xls,然后填上一些数据如图：

![img](/img/in-post/2017-09-27-python-excel-lua-json-xml/01.png)

2.来段代码简单遍历并输出

![img](/img/in-post/2017-09-27-python-excel-lua-json-xml/02.png)

3.执行`$ python test.py`

![img](/img/in-post/2017-09-27-python-excel-lua-json-xml/03.png)

** 可以看到终端上已经打印出了表中的所有单元格数据 **

本段完整代码文本如下：

```
# -*- coding: UTF-8 -*- 

# Author:Krisik
# 博客地址:http://www.cnblogs.com/krisirk/

import xlrd

workbook = xlrd.open_workbook("Configs.xls")
print "There are {} sheets in the workbook".format(workbook.nsheets)
for booksheet in workbook.sheets():
   for col in xrange(booksheet.ncols):              
       for row in xrange(booksheet.nrows):                      
           value = booksheet.cell(row, col).value
           print value
```

# 五、更进一步 - 按需求组合数据

我们已经得到了所有单元格数据，接下来就是按照项目的需求，组合成你需要的格式的文件，无论是lua还是json还是xml，都不是问题，下面我将以导出lua为例。

1.我将以第一行为Key，列的数据为Value进行排列，代码如下：

![img](/img/in-post/2017-09-27-python-excel-lua-json-xml/04.png)

执行`$ python test.py`后可以发现在同目录下创建了一个Configs.lua的文件，打开后显示:

![img](/img/in-post/2017-09-27-python-excel-lua-json-xml/05.png)

大功告成！

完整代码文本如下：

```
# -*- coding: UTF-8 -*- 

# Author:Krisik
# 博客地址:http://www.cnblogs.com/krisirk/

import xlrd

fileOutput = open("Configs.lua","w")

# 可以在这里写一些固定的注释代码之类的
writeData = "-- @author:kris


"

workbook = xlrd.open_workbook("Configs.xls")
print "There are {} sheets in the workbook".format(workbook.nsheets)

for booksheet in workbook.sheets():
       writeData = writeData + "AT" + booksheet.name + " = {
"
       for col in xrange(booksheet.ncols):
          for row in xrange(booksheet.nrows):
                   value = booksheet.cell(row, col).value
                       if  row == 0 :
                          writeData = writeData + "        " + "["" + value + ""]" + " = " + "{ "  
                  else :
                         writeData = writeData + """ + str(booksheet.cell(row, col).value) + "" , "
              else :
                 writeData = writeData + "} ,
"
       else :
        writeData = writeData + "}

"
else :
        fileOutput.write(writeData)

fileOutput.close()
```

# 六、最后

在实际项目中无论是Excel还是导出格式都会比示例中的更复杂，但是只要掌握了基本的核心功能，剩下的无非是耐心实现罢了。

# 七、终极需求

前文的实例都是比较基础的例子，现在来一个贴近真实项目的实例。

下图是要操作的表格：

![img](/img/in-post/2017-09-27-python-excel-lua-json-xml/06.png)

需求列表：

* Excel中导出的数据要有类型区分，比如int，string等
* 第一行为列的中文名称，这个名称只是给策划看，不导出
* 每张表右侧可以随便增加注释，注释不导出
* 需要字段ID和该ID在表中的index的一一对应数据，便于程序索引

# 解决方案

主要实现部分代码如图：

![img](/img/in-post/2017-09-27-python-excel-lua-json-xml/07.png)

# 处理结果

![img](/img/in-post/2017-09-27-python-excel-lua-json-xml/08.png)

执行脚本后打开生成的lua文件，可以看到完美解决了列出的需求。

# 全部代码

```
# -*- coding: UTF-8 -*- 

# Author:Krisik
# 博客地址:http://www.cnblogs.com/krisirk/

import xlrd

fileOutput = open("Configs.lua","w")

# 让py可以读取文件中的中文
import sysreload(sys)sys.setdefaultencoding("utf-8")# 可以在这里写一些固定的注释代码之类的writeData = "-- @author:kris


"

workbook = xlrd.open_workbook("Configs.xls")
print "There are {} sheets in the workbook".format(workbook.nsheets)

for booksheet in workbook.sheets():
 # print "Current Booksheet:[" + booksheet.name + "]"
      writeData = writeData + "AT" + booksheet.name + " = {
"

      # 处理id和index的特殊行
  writeData = writeData + "       " + "keys" + " = " + "{ " 
 for row in xrange(booksheet.nrows):              if  row <= 2 :
                       continue
               else :
                 Id    = str(int(booksheet.cell(row, 1).value))
                  index = str(int(booksheet.cell(row, 0).value))
                  writeData = writeData + "[" +Id + "] = " + index + " , "
   else:
         writeData = writeData + "} ,
"               

       for col in xrange(booksheet.ncols):              # 如果第二行为空，则此列为注释列               if not any(booksheet.cell(1,col).value) :                   continue                # 第三行为参数类型定义            colType = booksheet.cell(2, col).value          for row in xrange(booksheet.nrows):                        value = booksheet.cell(row, col).value
                 if      row == 0 or row == 2:
                            continue
                       elif  row == 1 :
                        writeData = writeData + "        " + "["" + value + ""]" + " = " + "{ "  
                   else :
                         if   colType == "int" :
                                 writeData = writeData + str(int(booksheet.cell(row, col).value)) + " , "
                        else : 
                                writeData = writeData + """ + str(booksheet.cell(row, col).value) + "" , "
               else :
                 writeData = writeData + "} ,
"
       else :
        writeData = writeData + "}

"
else :
        fileOutput.write(writeData)

fileOutput.close()
```

# 最后

到这里，关于操作excel的文章就结束了，相信对于绝大部分的需求或者更复杂的需求都能游刃有余了。

tips：通过代码生成Excel文件可以参考xlwt。

xlwt是python用来写excel的工具，跟xlrd对应。