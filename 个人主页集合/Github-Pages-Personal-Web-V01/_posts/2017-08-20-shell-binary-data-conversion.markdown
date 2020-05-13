---
layout:     post
title:      "Shell不同进制数据转换"
subtitle:   "shell binary data conversion"
date:       2017-08-20
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Linux系统运维与服务器管理
---

shell可以在不调用第3方命令，表示不同进制数据。这里总结以下表示方法。shell 脚本默认数值是由10 进制数处理,除非这个数字某种特殊的标记法或前缀开头. 才可以表示其它进制类型数值。如：以 0 开头就是 8 进制.以0x 开头就是16 进制数.使用 BASE#NUMBER 这种形式可以表示其它进制.BASE值：2-64.

# 其它进制转为10进制

## 八进制转十进制：

```
[chengmo@centos5 ~]$ ((num=0123));
[chengmo@centos5 ~]$ echo $num;
83
[chengmo@centos5 ~]$ ((num=8
#123));
[chengmo@centos5 ~]$ echo $num;
83
```

((表达式))，（（））里面可以是任意数据表达式。如果前面加入：”$”可以读取计算结果。

## 十六进制转十进制：

```
[chengmo@centos5 ~]$ ((num=0xff));
[chengmo@centos5 ~]$ echo $num;
255
[chengmo@centos5 ~]$ ((num=16
#ff));
[chengmo@centos5 ~]$ echo $num;
255
```

## base-32转十进制：

```
[chengmo@centos5 ~]$ ((num=32
#ffff));
[chengmo@centos5 ~]$ echo $num;
507375
```

## base64转十进制：

```
[chengmo@centos5 ~]$ ((num=64
#abc_));
[chengmo@centos5 ~]$ echo $num;
2667327
```

## 二进制转十进制

```
[chengmo@centos5 ~]$ ((num=2
#11111111));
[chengmo@centos5 ~]$ echo $num;
255
```

# 十进制转为其它进制

## 十进制转八进制

这里使用到：bc外部命令完成。bc命令格式转换为：echo “obase=进制;值”|bc

```
[chengmo@centos5 ~]$ echo "obase=8;01234567"|bc
4553207
```

## 二进制，十六进制，base64

```
[chengmo@centos5 ~]$ echo "obase=64;123456"|bc
30 09 00
```

shell内置各种进制表示方法非常简单。记得base#number 即可。这里记得赋值时候用(())符号。不能直接用=号了。=号没有值类型。默认将后面变成字符串了。如：

```
[chengmo@centos5 ~]$ num=0123;
[chengmo@centos5 ~]$ echo $num;
0123
```

0开头已经失去了意义了。

可以通过定义符：let达到(()) 运算效果。

```
[chengmo@centos5 ~]$ let num=0123;
[chengmo@centos5 ~]$ echo $num;
83
```