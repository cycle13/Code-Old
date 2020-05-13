---
layout:     post
title:      "linux shell 逻辑运算符和逻辑表达式"
subtitle:   "Shell logical operator"
date:       2017-08-26
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Linux系统运维与服务器管理
---

> shell的逻辑运算符 涉及有以下几种类型，因此只要适当选择，可以解决我们很多复杂的判断，达到事半功倍效果。

# 逻辑运算符

![img](/img/in-post/2017-08-26-shell-logical-operator/01.png)

![img](/img/in-post/2017-08-26-shell-logical-operator/02.png)

# 逻辑表达式

## test 命令

使用方法：test EXPRESSION

如：

```
[root~]# test 1 = 1 && echo 'ok'
ok
[root~]# test -d /etc/ && echo 'ok'
ok
[root~]# test 1 -eq 1 && echo 'ok'
ok
[root~]# if test 1 = 1 ; then echo 'ok'; fi
ok
```

注意：所有字符 与逻辑运算符直接用“空格”分开，不能连到一起。

## 精简表达式

### [] 表达式

```
[root~]# [ 1 -eq 1 ] && echo 'ok'
ok
[root~]# [ 2 < 1 ] && echo 'ok'
-bash: 2: No such file or directory
[root~]# [ 2 \< 1 ] && echo 'ok'
[root~]# [ 2 -gt 1 -a 3 -lt 4 ] && echo 'ok'
ok
[root~]# [ 2 -gt 1 && 3 -lt 4 ] && echo 'ok'
-bash: [: missing `]'
```

注意：在`[]` 表达式中，常见的`>`,`<`需要加转义字符，表示字符串大小比较，以acill码 位置作为比较。 不直接支持`<`,`>`运算符，还有逻辑运算符`||`,`&&`它需要用`-a[and]`, `–o[or]`表示

### [[]] 表达式

```
[root~]# [ 1 -eq 1 ] && echo 'ok'
ok
[root~]$ [[ 2 < 3 ]] && echo 'ok'
ok
[root~]$ [[ 2 < 3 && 4 > 5 ]] && echo 'ok'
ok
```

注意：`[[]]` 运算符只是`[]`运算符的扩充。能够支持`<`,`>`符号运算不需要转义符，它还是以字符串比较大小。里面支持逻辑运算符：`||`, `&&`

## 性能比较

bash的条件表达式中有三个几乎等效的符号和命令：`test`，`[]`和`[[]]`。通常，大家习惯用`if [];then`这样的形式。而`[[]]`的出现，根据ABS所说，是为了兼容`>`,`<`之类的运算符。以下是比较它们性能，发现`[[]]`是最快的。

```
$ time (for m in {1..100000}; do test -d .;done;)
real 0m0.658s
user 0m0.558s
sys 0m0.100s
$ time (for m in {1..100000}; do [ -d . ];done;)
real 0m0.609s
user 0m0.524s
sys 0m0.085s
$ time (for m in {1..100000}; do [[ -d . ]];done;)
real 0m0.311s
user 0m0.275s
sys 0m0.036s
```

不考虑对低版本bash和对sh的兼容的情况下，用`[[]]`是兼容性强，而且性能比较快，在做条件运算时候，可以使用该运算符。