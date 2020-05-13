---
layout:     post
title:      "Mac终端字体颜色修改"
subtitle:   "The font color of Mac terminal"
date:       2016-12-30
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Mac
    - 我的笔记
---

# bash_profile配置
`export LS_OPTIONS='--color=auto' # 如果没有指定，则自动选择颜色`<br/>
`export CLICOLOR='Yes' #是否输出颜色`<br/>
`export LSCOLORS='CxfxcxdxbxegedabagGxGx' #指定颜色`
其中，`LSCOLORS`中的意思是这样的：22个字母2个字母一组分别指定一种类型的文件或者文件夹显示的字体颜色和背景颜色。从第1组到第11组分别指定的文件或文件类型为：<br/>
directory<br/>
symbolic link<br/>
socket<br/>
pipe<br/>
executable<br/>
block special<br/>
character special<br/>
executable with setuid bit set<br/>
executable with setgid bit set<br/>
directory writable to others, with sticky bit<br/>
directory writable to others, without sticky bit<br/>
下面是对应的颜色：<br/>
a 黑色<br/>
b 红色<br/>
c 绿色<br/>
d 棕色<br/>
e 蓝色<br/>
f 洋红色<br/>
g 青色<br/>
h 浅灰色<br/>
A 黑色粗体<br/>
B 红色粗体<br/>
C 绿色粗体<br/>
D 棕色粗体<br/>
E 蓝色粗体<br/>
F 洋红色粗体<br/>
G 青色粗体<br/>
H 浅灰色粗体<br/>
x 系统默认颜色

# 改变PS1的颜色 

在PS1中配置字符序列颜色的格式为：`  \[\e[F;Bm\]`<br/>
基本上是夹在 "\e["（转义开方括号）和 "m" 之间数字值。假如指定一个以上的数字代码，则用分号将他们分开。其中 F 为字体颜色，编号30~37； B 为背景色，编号40~47。可通过 \e[0m 关闭颜色输出；特别的，当B为1时，将显示加亮加粗的文字，请看下面的颜色表和代码表。

颜色表

    前景 背景 颜色
    ---------------------------------------
    30 40 黑色
    31 41 红色
    32 42 绿色
    33 43 黄色
    34 44 蓝色
    35 45 紫色
    36 46 青色
    37 47 白色


    代码 意义
    -------------------------
    0 OFF
    1 高亮显示
    4 underline
    5 闪烁
    7 反白显示
    8 不可见