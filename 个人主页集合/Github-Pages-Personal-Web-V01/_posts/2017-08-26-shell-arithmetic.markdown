---
layout:     post
title:      "linux shell 实现四则运算（整数及浮点）简单方法"
subtitle:   "Shell arithmetic"
date:       2017-08-26
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Linux系统运维与服务器管理
---

# 简单方法

```
[chengmo@centos5 ~]$ b=$((5*5+5-3/2))
[chengmo@centos5 ~]$ echo $b
29
```

在linux shell中，我们可以使用 `$(())` 将表达式放在括号中，即可达到运算的功能。

# 其它方法

用：expr 实现运算

```
[chengmo@centos5 ~]$ expr 5 - 4
```

注意：将需要运算的表达式写入在expr 后面即可，保证 参数与运算符号中间有空格隔开。

![img](/img/in-post/2017-08-25-shell-arithmetic/01.png)

# 浮点运算

```
[chengmo@centos5 ~]$ expr 5.0 - 4
```

expr: 非法参数

```
[chengmo@centos5 ~]$ echo $((5.0-4))
-bash: 5.0-4: syntax error in expression (error token is ".0-4")
```

从上面运算结果，看来上面表达式，不足以支持浮点运算了。查阅资料才发现：bash 不支持浮点运算，如果需要进行浮点运算，需要借助bc,awk 处理。

方法一：

```
[chengmo@centos5 ~]$ c=$(echo "5.01-4*2.0"|bc)
[chengmo@centos5 ~]$ echo $c
-2.99
```

方法二：

```
[chengmo@centos5 ~]$ c=$(awk 'BEGIN{print 7.01*5-4.01 }')
[chengmo@centos5 ~]$ echo $c
31.04
```

注：在shell 中$() 与 “等效。 中间包含命令语句执行，返回执行结果。