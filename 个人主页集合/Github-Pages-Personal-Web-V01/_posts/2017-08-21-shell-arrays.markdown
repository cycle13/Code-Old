---
layout:     post
title:      "Shell中的数组"
subtitle:   "Arrays in shell"
date:       2017-08-21
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Linux系统运维与服务器管理
---

# 数组定义

```
[chengmo@centos5 ~]$ a=(1 2 3 4 5)
[chengmo@centos5 ~]$ echo $a
1
```

一对括号表示是数组，数组元素用“空格”符号分割开。

# 数组读取与赋值

得到长度：

```
[chengmo@centos5 ~]$ echo ${
#a[@]}
5
```

用${#数组名[@或*]} 可以得到数组长度

读取：

```
[chengmo@centos5 ~]$ echo ${a[2]}
3
[chengmo@centos5 ~]$ echo ${a[*]}
1 2 3 4 5
```

用${数组名[下标]} 下标是从0开始 下标是：*或者@ 得到整个数组内容

赋值:

```
[chengmo@centos5 ~]$ a[1]=100
[chengmo@centos5 ~]$ echo ${a[*]}
1 100 3 4 5
[chengmo@centos5 ~]$ a[5]=100
[chengmo@centos5 ~]$ echo ${a[*]}
1 100 3 4 5 100
```

直接通过 数组名[下标] 就可以对其进行引用赋值，如果下标不存在，自动添加新一个数组元素

删除:

```
[chengmo@centos5 ~]$ a=(1 2 3 4 5)
[chengmo@centos5 ~]$ unset a
[chengmo@centos5 ~]$ echo ${a[*]}
[chengmo@centos5 ~]$ a=(1 2 3 4 5)
[chengmo@centos5 ~]$ unset a[1]
[chengmo@centos5 ~]$ echo ${a[*]}
1 3 4 5
[chengmo@centos5 ~]$ echo ${
#a[*]}
4
```

直接通过：unset 数组[下标] 可以清除相应的元素，不带下标，清除整个数据。

## 特殊使用

分片:

```
[chengmo@centos5 ~]$ a=(1 2 3 4 5)
[chengmo@centos5 ~]$ echo ${a[@]:0:3}
1 2 3
[chengmo@centos5 ~]$ echo ${a[@]:1:4}
2 3 4 5
[chengmo@centos5 ~]$ c=(${a[@]:1:4})
[chengmo@centos5 ~]$ echo ${
#c[@]}
4
[chengmo@centos5 ~]$ echo ${c[*]}
2 3 4 5
```

直接通过 ${数组名[@或*]:起始位置:长度} 切片原先数组，返回是字符串，中间用“空格”分开，因此如果加上”()”，将得到切片数组，上面例子：c 就是一个新数据。

替换:

```
[chengmo@centos5 ~]$ a=(1 2 3 4 5)
[chengmo@centos5 ~]$ echo ${a[@]/3/100}
1 2 100 4 5
[chengmo@centos5 ~]$ echo ${a[@]}
1 2 3 4 5
[chengmo@centos5 ~]$ a=(${a[@]/3/100})
[chengmo@centos5 ~]$ echo ${a[@]}
1 2 100 4 5
```

调用方法是：${数组名[@或*]/查找字符/替换字符} 该操作不会改变原先数组内容，如果需要修改，可以看上面例子，重新定义数据。