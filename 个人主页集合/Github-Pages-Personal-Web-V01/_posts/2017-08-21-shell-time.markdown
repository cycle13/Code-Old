---
layout:     post
title:      "Shell中的时间运算以及时间差计算方法"
subtitle:   "Time operations in shell"
date:       2017-08-21
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Linux系统运维与服务器管理
---

# 时间加减

这里处理方法，是将基础的时间转变为时间戳，然后，需要增加或者改变时间，变成 秒。

如：1990-01-01 01:01:01 加上 1小时 20分

处理方法：

1. 将基础时间转为时间戳

```
time1=$(date +%s -d '1990-01-01 01:01:01')
echo $time1
```

631126861 【时间戳】

2. 将增加时间变成秒

```
# time2=$((1*60*60+20*60))
# echo $time2
```

4800

3. 两个时间相加，计算出结果时间

```
time1=$(($time1+$time2))
time1=$(date +%Y-%m-%d\ %H:%M:%S -d "1970-01-01 UTC $time1 seconds");
echo $time1
```

1990-01-01 02:21:01

# 时间差计算方法

如：2010-01-01 与 2009-01-01 11:11:11 时间差

原理：同样转成时间戳，然后计算天，时，分，秒

```
time1=$(($(date +%s -d '2010-01-01') - $(date +%s -d '2009-01-01 11:11:11')));
echo time1
```

将time1 / 60 秒，就变成分了。

补充说明：

shell 单括号运算符号：
```
a=$(date);
```
等同于：
```
a=`date`;
```

双括号运算符:

```
a=$((1+2));

echo $a;
```
等同于：
```
a=`expr 1 + 2`
```