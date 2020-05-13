---
layout:     post
title:      "Intel编译器优化"
subtitle:   "Intel compiler optimization"
date:       2016-12-21
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - 编译器
---

## 普通优化

```
-O0                 禁止优化
-g                  生成标记
-O1                 优化二进制代码
-O2                 优化速度（默认）
-O3                 优化数据缓存
```

## 过程间优化

```
-ip                 优化编译单个文件
-ipo                通过内联函数优化交叉编译多个文件
```

## 自动并行优化

```
-parallel           对某些代码做自动并行优化
-par_report[n]      记录优化过程，汇报结果
```

## 基于CPU的矢量化优化

```
-Xw                 为Pentium4等支持MMX、SSE和SSE2指令的处理器做专门优化
-xP，-axP           为Core等支持MMX、SSE、SSE2和SSE3指令的处理器做专门优化
```

## OpenMP优化

```
-openmp             打开OpenMP优化功能
-openmp-report      提供优化报告、错误
```

## 支持Intel线程检查器的编译

```
-tcheck             支持线程检查器检测线程
```

## 代码内优化

> 前面介绍的都是利用编译器的优化选项开关来进行性能的优化，一般这些优化选项是针对所有代码或者针对某个源文件的所有函数时。但是很多时候我们仅仅希望代码中对某个特定部分进行优化，这时候就需要利用到关键字pragma。虽然pragma是C语言的关键字，但是它的意义和作用是由使用的编译器来解释的。这样，虽然利用pragma优化会很方便，但是同时也会造成代码可能只能绑定在特定编译器上，为代码的移植带来困难。

- ## pragma使用

```
#pragma <pragma name> 

#pragma optimize("",on|off)                      打开和关闭优化支持

#pragma optimization_level n                     控制采用的优化级别

#pragma loop count min=n,max=n,avg=n             告诉编译器循环估计的执行次数
#pragma loop count = n 
#pragma loop count =n1[,n2]…            

#pragma nounroll                                 告诉编译器不要展开循环

#pragma unroll                                   告诉编译器循环展开的次数
#pragma unroll(n)

#pragma distribute point                         控制循环分割

#pragma ivdep                                    告诉编译器没有数据依赖

#pragma novector                                 告诉编译器不要把循环自动矢量化

#pragma vector {aligned|unaligned|always}        怎么样自动矢量化，哪些影响矢量化选择的因素可以忽略掉

#pragma vector nontemporal                       自动矢量化的代码中采用流式存储
```

- ## pragma范例与简单说明

> 从上一部分可以看到，pragma可以分为几种，一种是控制是否优化，优化到什么程度，一种是对循环优化，还有一种是是否向量化。下面给出一些简单的示例与说明。

- ### 优化控制

\#pragma optimize("",on\|off)用于打开和关闭优化支持。\#pragma optimization_level n用于控制采用的优化级别，n取值为0到3，分别对应着自动化编译选项中的-Od、-O1、-O2、-O3。

<pre class="prettyprint lang-c linenums">
#pragma optimize("", off)   不使用优化
void hello01(){...}

#pragma optimize("", on)    使用优化，同时开启优化程度为1，对应-O1
#pragma optimization_level 1 
void hello02(){...}
</pre>

- ### 循环优化

\#pragma loop_count min=n,max=n,avg=n，用于告诉编译器下面的循环估计最少执行几次、最大执行几次和平均执行几次，显然这个pragma可以帮助编译器决定是否要进行循环展开，是否要进行自动矢量化等，有助于进一步代码优化。\#pragma nounroll用于告诉编译器不要对这个循环进行循环展开，\#pragma unroll(n)用于告诉编译器这个循环可以进行循环展开，循环展开的次数最多为n次。如果n等于0，则表示不进行循环展开，而采用\#pragma unroll表示由编译器决定循环展开的次数。编译器的选项开关中也提供了循环展开的控制（-Qunroll，-Qunroll:n）。如果循环之前包含\#pragma unroll，则它会优先处理pragma。\#pragma distribute point，它用于控制循环分割，可以放在循环之前也可以放在循环体中。如果放在循环体之前，表示由编译器来决定如何对循环进行分割，而放在循环体中，则告诉编译器循环体从pragma所在的位置分割为两个循环。

<pre class="prettyprint lang-c linenums">
void hello01() {
#pragma unroll(5)    循环展开5次 
for (int i = 1; i < 100; i++) {
...
} 
}

for (i=0; i< 100; i++) { 
a[i] = a[i] +i; 
b[i] = b[i] +i; 
#pragma distribute point 
x[i] = x[i] +i;
y[i] = y[i] +i; 
}
所以这部分循环相当于这样两个循环：
for (i=0; i< 100; i++) {
a[i] = a[i] +i; 
b[i] = b[i] +i; 
}
for (i=0; i< 100; i++) {
x[i] = x[i] +i;
y[i] = y[i] +i; 
} 
</pre>

- ### 向量化

如果一个循环有数据依赖或者编译器无法确定是否具有数据依赖，为了安全起见编译器会选择不进行自动矢量化。\#pragma novector用于告诉编译器紧接着的循环即便可以自动矢量化也不要进行；\#pragma ivdep用于告诉编译器紧接着的循环没有数据依赖，可以进行矢量化。但是一个循环没有数据依赖并不代表其一定会被自动矢量化，可能还要考虑到自动矢量化的开销、内存访问是否步长、数据是否对齐等情况，\#pragma vector用于帮助编译器来决定是否对于没有数据依赖的循环进行自动矢量化。其中\#pragma vector always表示只要可以就进行自动矢量化，而不考虑性能方面的因素。\#pragma vector {aligned \| unaligned}用于告诉紧接着的循环体对于内存的访问是否是对齐的，如果是对齐的，则在其他条件（没有数据依赖）满足的情况下可以进行矢量化。另外_assume_aligned(var,n)也可用来告诉变量var是n比特对齐的。\#pragma vector nontemporal用于告诉编译器在生成自动矢量化的代码时进行流式存储，即把数据直接写到内存中而不用通过缓存。

<pre class="prettyprint lang-c linenums">
void hello(){
#pragma ivdep 
for (i = 0; i < 100; i++){
a[i]=a[i+x];    这里我知道i+x不会出现小于零的情况，没有数据依赖，所以告诉编译器可以进行矢量化 	
} 
}

#pragma vector aligned  或者__assume_aligned(a,16); __assume_aligned(b,16);
这里说明a数组和b数组已经实现16比特对齐
for (i = 0; i < 100; i++) {
a[i] = b[i] + 2.0;
}

#pragma vector nontemporal  
for (i = 0; i < 100; i++) {
a[i] = 1; 
} 
</pre>