---
layout:     post
title:      "Shell自定义函数"
subtitle:   "shell custom function"
date:       2017-08-20
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Linux系统运维与服务器管理
---

linux shell 可以用户定义函数，然后在shell脚本中可以随便调用。下面说说它的定义方法，以及调用需要注意那些事项。

# 定义shell函数(define function)

语法：

```
[ function ] funname [()]
{
action;
[return int;]
}
```

说明：

1. 可以带function fun() 定义，也可以直接fun() 定义,不带任何参数。

2. 参数返回，可以显示加：return 返回，如果不加，将以最后一条命令运行结果，作为返回值。 return后跟数值n(0-255)

实例（testfun1.sh）：

```
#!/bin/sh
fSum 3 2;
function fSum()
{
echo $1,$2;
return $(($1+$2));
}
fSum 5 7;
total=$(fSum 3 2);
echo $total,$?;
sh testfun1.sh
testfun1.sh: line 3: fSum: command not found
5,7
3,2
5
```

从上面这个例子我们可以得到几点结论：

1. 必须在调用函数地方之前，声明函数，shell脚本是逐行运行。不会像其它语言一样先预编译。一次必须在使用函数前先声明函数。

2. total=$(fSum 3 2); 通过这种调用方法，我们清楚知道，在shell 中 单括号里面，可以是：命令语句。 因此，我们可以将shell中函数，看作是定义一个新的命令，它是命令，因此 各个输入参数直接用 空格分隔。 一次，命令里面获得参数方法可以通过：$0…$n得到。 $0代表函数本身。

3. 函数返回值，只能通过$? 系统变量获得，直接通过=,获得是空值。其实，我们按照上面一条理解，知道函数是一个命令，在shell获得命令返回值，都需要通过$?获得。

# 函数作用域，变量作用范围

先我们看一个实例(testfun2.sh )：

```
#!/bin/sh
echo $(uname);
declare num=1000;
uname()
{
echo "test!";
((num++));
return 100;
}
testvar()
{
local num=10;
((num++));
echo $num;
}
uname;
echo $?
echo $num;
testvar;
echo $num;
sh testfun2.sh
Linux
test!
100
1001
11
1001
```

我们一起来分析下上面这个实例，可以得到如下结论：

1. 定义函数可以与系统命令相同，说明shell搜索命令时候，首先会在当前的shell文件定义好的地方查找，找到直接执行。

2. 需要获得函数值：通过$?获得

3. 如果需要传出其它类型函数值，可以在函数调用之前，定义变量（这个就是全局变量）。在函数内部就可以直接修改，然后在执行函数就可以读出修改过的值。

4. 如果需要定义自己变量，可以在函数中定义：local 变量=值 ，这时变量就是内部变量，它的修改，不会影响函数外部相同变量的值 。

