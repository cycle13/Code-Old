---
layout:     post
title:      "GCC简易使用说明"
subtitle:   "GCC simple guide"
date:       2016-12-16
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - 编译器
    - C/C++
---

> GCC是GNU C Compiler的缩写。经过很多年的发展，GCC已经不单单支持C语言了，也支持例如Fortran，C++等等，有人把GCC现在解释成GNU Compiler Collection。GCC是开源的，所以在很多场合得到广泛的应用。


## 示例

用最简单的Helloworld程序作为示例：

<pre class="prettyprint lang-c linenums">
//helloworld.c 
#include < stdio.h > 
int main(void) 
{ 
    printf("Hello World!\n"); 
    return 0; 
}
</pre>

最简单的编译指令是：`gcc helloworld.c  -o helloworld`<br/>
执行：`./helloworld`<br/>
这个简单的过程中包含四个部分：预处理、编译、汇编和连接。


## 预处理

预处理实际上是把代码中一些部分在编译前替换成原始的代码。命令：<br/>
`gcc -E helloworld.c -o helloworld.i` <br/>
或 <br/>
`gcc -E helloworld.c`<br/>
helloworld.i文件中存放着helloworld.c经预处理之后的代码。<br/>
第二条命令则是直接在命令行窗口中输出预处理后的代码。<br/>
`-E`选项，可以让编译器在预处理后停止，并输出预处理结果。<br/>
在我们这个例子里，就是把stdio.h文件中的内容插入到helloworld.c文件中了。


## 编译

预处理之后，可直接对生成的helloworld.i文件编译，生成汇编代码：<br/>
`gcc -S helloworld.i -o helloworld.s`<br/>
`-S`选项，表示在程序编译期间，在生成汇编代码后，停止，`-o`输出汇编代码文件。

## 汇编

把编译生成的汇编代码文件编译为目标文件：<br/>
`gcc -c helloworld.s -o helloworld.o`<br/>
这里起作用的主要是GAS汇编器。

## 连接

gcc连接器是gas提供的，负责将程序的目标文件与依赖的库文件(静态连接库和动态连接库)连接起来，最终生成可执行文件。<br/>
汇编生成的helloworld.o，将其与Ｃ标准输入输出库进行连接，最终生成程序helloworld.exe<br/>
`gcc helloworld.o -o helloworld.exe`<br/>
然后就可以在命令行窗口执行：<br/>
`./helloworld.exe`

## 编译一个程序的多个文件

通常整个程序是由多个源文件组成的，相应地也就形成了多个编译单元。可以使用如下命令编译：<br/>
`gcc hello01.c hello02.c -o hello.exe`<br/>
如果同时处理的文件不止一个，GCC仍然会按照预处理、编译和链接的过程依次进行，最终生成一个可执行文件。

## 错误检查与处理

可以采用命令`gcc -pedantic hello.c -o hello`<br/>
-pedantic 选项能够帮助程序员发现一些不符合ANSI/ISO C标准的代码，但不是全部，事实上只有ANSI/ISO C语言标准中要求进行编译器诊断的那些情况，才有可能被GCC发现并提出警告。

也可以采用：<br/>
`gcc -Wall hello.c -o hello`<br/>
`-Wall`会让GCC尽可能多地写出警告信息。

在编译程序时带上`-Werror`选项，那么GCC会在所有产生警告的地方停止编译，也有助于调试代码。

## 库文件

一般需要调用的有两种，一种在include文件夹下，为头文件；另一种在lib文件夹下，为二进制so文件<br/>
`gcc -L/home/qqf/netcdf/lib –lnetcdf -I/home/qqf/netcdf/include hello.o –o hello`<br/>
Linux下的库文件分为两大类分别是动态链接库（通常以.so结尾）和静态链接库（通常以.a结尾），二者的区别仅在于程序执行时所需的代码是在运行时动态加载的，还是在编译时静态加载的。

- ## 强制链接时使用静态链接库

默认情况下， GCC在链接时优先使用动态链接库，只有当动态链接库不存在时才考虑使用静态链接库，如果需要的话可以在编译时加上`-static` 选项，强制使用静态链接库。<br/>
`gcc -L/home/qqf/netcdf/lib –lnetcdf –static -I/home/qqf/netcdf/include hello.o –o hello`<br/>

- ## 静态库链接时搜索路径顺序

1.ld会去找GCC命令中的参数-L<br/>
2.找GCC的环境变量LIBRARY_PATH<br/>
3.找内定目录/lib,/usr/lib,/usr/local/lib,这是当初编译GCC时写在程序内的

- ## 动态链接与执行时搜索路径顺序

1.编译目标代码时指定的动态库搜索路径<br/>
2.环境变量LD_LIBRARY_PATH指定的动态库搜索路径<br/>
3.配置文件/etc/ld.so.conf 中指定的动态库搜索路径<br/>
4.默认的动态库搜索路径/lib<br/>
5.默认的动态库搜索路径/usr/lib<br/>

- ## 有关环境变量

LIBRARY_PATH环境变量：指定程序静态链接库文件搜索路径<br/>
LD_LIBRARY_PATH环境变量：指定程序动态链接库文件搜索路径

## 链接选项和路径

现代连接器在处理动态库时将链接时路径（Link-time path）和运行时路径（Run-time path）分开,用户可以通过`-L`指定连接时库的路径，通过`-R`（或`-rpath`）指定程序运行时库的路径，大大提高了库应用的灵活性。比如我们做嵌入式移植时`#arm-linux-gcc $(CFLAGS) –o target –L/work/lib/zlib/ -llibz-1.2.3` (work/lib/zlib下是交叉编译好的zlib库)，将target编译好后我们只要把zlib库拷贝到开发板的系统默认路径下即可。或者通过`-rpath`（或`-R` ）、LD_LIBRARY_PATH指定查找路径。

链接器ld的选项有`-L`，`-rpath` 和 `-rpath-link`，看了下`man ld`，大致是这个意思：<br/>
`-L`: "链接"的时候，去找的目录，也就是所有的 `-lFOO` 选项里的库，都会先从 `-L` 指定的目录去找，然后是默认的地方。编译时的-L选项并不影响环境变量LD_LIBRARY_PATH，`-L`只是指定了程序编译连接时库的路径，并不影响程序执行时库的路径，系统还是会到默认路径下查找该程序所需要的库，如果找不到，还是会报错，类似cannot open shared object file。<br/>
`-rpath-link`：这个也是用于"链接"的时候的，例如你显示指定的需要FOO.so，但是FOO.so 本身是需要BAR.so 的，后者你并没有指定，而是FOO.so引用到它，这个时候，会先从`-rpath-link`给的路径里找。</br>
`-rpath`: "运行"的时候，去找的目录。运行的时候，要找 .so文件，会从这个选项里指定的地方去找。对于交叉编译，交叉编译链接器需已经配置`–with-sysroot`选项才能起作用。也就是说，`-rpath`指定的路径会被记录在生成的可执行程序中，用于运行时查找需要加载的动态库。`-rpath-link`则只用于链接时查找。

## 链接搜索顺序

The linker uses the following search paths to locate required shared libraries:<br/>
1.Any directories specified by `-rpath-link` options.<br/>
2.Any directories specified by `-rpath` options. The difference between `-rpath` and `-rpath-link` is that directories specified by `-rpath` options are included in the executable and used at runtime, whereas the `-rpath-link` option is only effective at link time. Searching `-rpath` in this way is only supported by native linkers and cross linkers which have been configured with the `--with-sysroot` option.<br/>
3.On an ELF system, for native linkers, if the `-rpath` and `-rpath-link` options were not used, search the contents of the environment variable "LD_RUN_PATH".<br/>
4.On SunOS, if the -rpath option was not used, search any directories specified using `-L` options.<br/>
5.For a native linker, the search the contents of the environment variable "LD_LIBRARY_PATH".<br/>
6.For a native ELF linker, the directories in "DT_RUNPATH" or "DT_RPATH" of a shared library are searched for shared libraries needed by it. The "DT_RPATH" entries are ignored if "DT_RUNPATH" entries exist.<br/>
7.The default directories, normally `/lib` and `/usr/lib`.<br/>
8.For a native linker on an ELF system, if the file /etc/ld.so.conf exists, the list of directories found in that file.<br/>
If the required shared library is not found, the linker will issue a warning and continue with the link.  

## gcc和链接选项的使用

在gcc中使用ld链接选项时，需要在选项前面加上前缀-Wl，以区别不是编译器的选项。<br/>
if the linker is being invoked indirectly, via a compiler driver (e.g. gcc) then all the linker command line options should be prefixed by -Wl, (or whatever is appropriate for the particular compiler driver) like this:<br/>
`gcc -Wl,--start-group foo.o bar.o -Wl,--end-group`<br/>
This is important, because otherwise the compiler driver program may silently drop the linker options, resulting in a bad link.



