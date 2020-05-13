---
layout:     post
title:      "Shell中的双括号运算符"
subtitle:   "Double brackets in shell"
date:       2017-08-21
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Linux系统运维与服务器管理
---

> 在刚开始学习inux shell脚本编程时候，对于它的 四则运算以及逻辑运算，估计很多朋友都感觉比较难以接受。特变逻辑运算符”[]”使用时候，必须保证运算符与算数 之间有空格。 四则运算也只能借助：let,expr等命令完成。 今天讲的双括号”(())”结构语句，就是对shell中算数及赋值运算的扩展。

使用方法：

语法：

（（表达式1,表达式2…））

特点：

1. 在双括号结构中，所有表达式可以像c语言一样，如：a++,b–等。

2. 在双括号结构中，所有变量可以不加入：“$”符号前缀。

3. 双括号可以进行逻辑运算，四则运算

4. 双括号结构 扩展了for，while,if条件测试运算

5. 支持多个表达式运算，各个表达式之间用“，”分开

使用实例：

扩展四则运算

```
#!/bin/sh
a=1;
b=2;
c=3;
((a=a+1));
echo $a;
a=$((a+1,b++,c++));
echo $a,$b,$c
```

运行结果：

```
sh testsh.sh
2
3,3,4
```

双括号结构之间支持多个表达式，然后加减乘除等c语言常用运算符都支持。如果双括号带：$，将获得表达式值，赋值给左边变量。

扩展逻辑运算

```
#!/bin/sh
a=1;
b="ab";
echo $((a>1?8:9));
((b!="a"))&& echo "err2";
((a<2))&& echo "ok";
```

运行结果：

```
sh testsh.sh
9
err2
ok
```

扩展流程控制语句（逻辑关系式）

```
#!/bin/sh
num=100;
total=0;
for((i=0;i<=num;i++));
do
((total+=i));
done
echo $total;
total=0;
i=0;
while((i<=num));
do
((total+=i,i++));
done
echo $total;
if((total>=5050));then
echo "ok";
fi
```

运算结果：

```
sh testsh.sh
5050
5050
ok
```

有了双括号运算符：[[]],[],test 逻辑运算，已经let,expr 都可以抛到一边了。