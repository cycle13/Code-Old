---
layout:     post
title:      "NetCDF说明"
subtitle:   "Simple instruction for NetCDF"
date:       2017-02-03
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - NetCDF
---

# 概述

NetCDF全称为network Common Data Format，中文译法为“网络通用数据格式”，对程序员来说，它和zip、jpeg、bmp文件格式类似，都是一种文件格式的标准。netcdf文件开始的目的是用于存储气象科学中的数据，现在已经成为许多数据采集软件的生成文件的格式。从数学上来说，netcdf存储的数据就是一个多自变量的单值函数。用公式来说就是f(x,y,z,...)=value,函数的自变量x,y,z等在netcdf中叫做维(dimension)或坐标轴(axix),函数值value在netcdf中叫做变量(Variables).而自变量和函数值在物理学上的一些性质，比如计量单位(量纲)、物理学名称等等在netcdf中就叫属性(Attributes).

# NetCDF的下载

netcdf的是官方网站为http://www.unidata.ucar.edu/software/netcdf/。在本文中，我们主要讨论在windows平台上使用netcdf软件库。我们将要从这个网站上下载如下资源：<br/>
⑴netcdf的源代码，目前的地址为ftp://ftp.unidata.ucar.edu/pub/netcdf<br/>
⑵netcdf的在windows平台预编译好的dll，地址为ftp://ftp.unidata.ucar.edu/pub/netcdf/contrib/win32/netcdf-3.6.1-win32.zip<br/>
解压后里面有如下东西<br/>
netcdf.dll 为编译好的dll<br/>
ncgen.exe 为生成netcdf文件的工具<br/>
ncdump.exe 为读取netcdf文件的工具<br/>
netcdf.lib 和 netcdf.exp在编程时会用到，后面会讲。<br/>
⑶netcdf的相关文档，下载地址为http://www.unidata.ucar.edu/software/netcdf/docs<br/>
下面我们来看netcdf文件的具体内容。

# NetCDF文件的内容

## 变量(Variables)

变量对应着真实的物理数据。比如我们家里的电表，每个时刻显示的读数表示用户的到该时刻的耗电量。这个读数值就可以用netcdf里的变量来表示。它是一个以时间为自变量（或者说自变量个数为一维）的单值函数。再比如在气象学中要作出一个气压图，就是“东经xx度，北纬yy度的点的大气压值为多少帕”，这是一个二维单值函数，两维分别是经度和纬度。函数值为大气压。从上面的例子可以看出，netcdf中的变量就是一个N维数组，数组的维数就是实际问题中的自变量个数，数组的值就是观测得到的物理值。变量（数组值）在netcdf中的存储类型有六种，ascii字符(char) ,字节(byte), 短整型(short), 整型(int), 浮点(float), 双精度(double). 显然这些类型和c中的类型一致，搞C的朋友应该很快就能明白。

## 维(dimension)

一个维对应着函数中的某个自变量，或者说函数图象中的一个坐标轴，在线性代数中就是一个N维向量的一个分量（这也是维这个名称的由来）。在netcdf中，一个维具有一个名字和范围（或者说长度，也就是数学上所说的定义域，可以是离散的点集合或者连续的区间）。在netcdf中,维的长度基本都是有限的，最多只能有一个具有无限长度的维。

## 属性(Attribute)

属性对变量值和维的具体物理含义的注释或者说解释。因为变量和维在netcdf中都只是无量纲的数字，要想让人们明白这些数字的具体含义，就得靠属性这个对象了。在netcdf中，属性由一个属性名和一个属性值（一般为字符串）组成。比如，在某个cdl文件(cdl文件的具体格式在下一节中讲述)中有这样的代码段temperature:units = "celsius" ;前面的temperature是一个已经定义好的变量（Variable），即温度，冒号后面的units就是属性名，表示物理单位，=后面的就是units这个属性的值，为“celsius” ，即摄氏度，整个一行代码的意思就是温度这个物理量的单位为celsius，很好理解。

# CDL结构

CDL全称为network Common data form Description Language，它是用来描述netcdf文件的结构的一种语法格式。它包括前面所说的三种netcdf对象(变量、维、属性)的具体定义。
看一个具体例子（这个例子cdl文件是从netcdf教程中的2.1节The simple xy Example摘出来的）

```
netcdf simple_xy {
dimensions:
x = 6 ;
y = 12 ;
variables:
int data(x, y) ;
data:
data =
0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71 ;
}
```

上面的代码定义了一个符合netcdf格式的结构simple_xy,
这个结构包括三个部分<br/>
1、维的定义，以dimensions:关键字开头

```
dimensions:
x = 6 ;
y = 12 ;
```

定义了两个轴（或者说两维），名字分别为x和y,x轴的长度（准确的说是坐标点的个数）为6，y轴的长度为12。<br/>
2、变量的定义:以variables:开头

```
variables:
int data(x, y);
```

定义了一个以x轴和y轴为自变量的函数data，数学公式就是f(x,y)=data;<br/>
注意维出现的顺序是有序的，它决定data段中的具体赋值结果.<br/>
3、数据的定义，以data:开头

```
data:
data =
0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71 ;
```

这个段数据用数学的函数公式f(x,y)=data来看，就是 x=0,y=0时，data = 0;x=0,y=1时，data = 1;x=5,y=11是，data=71;<br/>
要注意的是，<br/>
1、赋值顺序：<br/>
我们给出的是c格式的cdl文件，因此这里的赋值顺序和c语言中的一致，也就是通常所说的“行式赋值”，而fortran语言中则是“列式赋值”，因此在fortran格式的cdl文件中，data段的数值顺序和这里正好行列互换。<br/>
2、自变量的默认取值和坐标变量<br/>
如果只给出维的长度，那么维的值默认从0开始，然后自动加1，到(长度-1)停止，很多情况下我们要自己给出每个点的坐标值，这时就需要用到netcdf里的坐标变量"coordinate varibles"：增加一个和只和维相关的一元函数（自变量）并给出它的取值范围。比如下面的cdl文件（摘自netcdf教程中的2.2 The sfc pres temp Example）

```
netcdf sfc_pres_temp {
dimensions:
latitude = 6 ; //纬度轴
longitude = 12 ; //经度轴
variables:
float latitude(latitude) ; //坐标变量，存储具体纬度
latitude:units = "degrees_north" ;
float longitude(longitude) ; //坐标变量，存储具体纬度
longitude:units = "degrees_east" ;
float pressure(latitude, longitude) ; //某个点（经度和纬度的交点）的大气压值
pressure:units = "hPa" ; //大气压的单位为
float temperature(latitude, longitude) ; //某个点（经度和纬度的交点）的温度值
temperature:units = "celsius" ; //温度的单位为
data:
latitude = 25, 30, 35, 40, 45, 50 ;
longitude = -125, -120, -115, -110, -105, -100, -95, -90, -85, -80, -75, -70 ;
pressure =
900, 906, 912, 918, 924, 930, 936, 942, 948, 954, 960, 966,
901, 907, 913, 919, 925, 931, 937, 943, 949, 955, 961, 967,
902, 908, 914, 920, 926, 932, 938, 944, 950, 956, 962, 968,
903, 909, 915, 921, 927, 933, 939, 945, 951, 957, 963, 969,
904, 910, 916, 922, 928, 934, 940, 946, 952, 958, 964, 970,
905, 911, 917, 923, 929, 935, 941, 947, 953, 959, 965, 971 ;
temperature =
9, 10.5, 12, 13.5, 15, 16.5, 18, 19.5, 21, 22.5, 24, 25.5,
9.25, 10.75, 12.25, 13.75, 15.25, 16.75, 18.25, 19.75, 21.25, 22.75, 24.25,
25.75,
9.5, 11, 12.5, 14, 15.5, 17, 18.5, 20, 21.5, 23, 24.5, 26,
9.75, 11.25, 12.75, 14.25, 15.75, 17.25, 18.75, 20.25, 21.75, 23.25, 24.75,
26.25,
10, 11.5, 13, 14.5, 16, 17.5, 19, 20.5, 22, 23.5, 25, 26.5,
10.25, 11.75, 13.25, 14.75, 16.25, 17.75, 19.25, 20.75, 22.25, 23.75,
25.25
```

对于上面的数据，就是
latitude = 25，longitude = -125时，pressure = 900，temperature = 9;
latitude = 25，longitude = -120时，pressure = 906，temperature = 10.5;
以此类推。

# netcdf文件的读写

“学以致用” ，前面讲的都是netcdf的基本知识，都是为了本节的核心问题——读写netcdf格式的文件做铺垫之用，下面我们就来看看如何建立一个netcdf格式文件，以及如何再读出它的内容。

1、在命令行下读写netcdf文件<br/>
⑴建立一个simple_xy.cdl文件，内容就是上一节“CDL结构”中的第一个例子。<br/>
⑵用ncgen.exe工具（下载地址见前面的第二节）建立netcdf文件<br/>
①将ncgen所在目录加到系统path变量中或者直接将ncgen.exe拷到simple_xy.cdl所在目录下<br/>
②执行ncgen -o simple_xy.nc simple_xy.cdl生成netcdf格式文件simple_xy.nc<br/>
⑶生成的simple_xy.nc是一个二进制文件，要想从这个文件中还原出数据信息，就要用ncdump工具<br/>
①将ncdump所在目录加到系统path变量中或者直接将ncdump.exe拷到simple_xy.nc所在目录下<br/>
②在命令行下执行ncdump simple_xy.nc，这时屏幕的输出和simple_xy.cdl内容完全一样。说明我们的文件读写操作都是正确的。

2、编程读写netcdf文件
前面我们知道如何手工去建立和读取netddf文件，下面我们来看看如何在程序中用代码实现netcdf文件的建立和分析。我的编程环境为win2000+vc6.0 并安装了vc sp6补丁包。例子代码选自netcdf教程中的2.1节The simple xy Example<br/>
⑴将netcdf的源代码解压。我们将用到里面的libsrc/netcdf.h头文件<br/>
⑵在vc6中建立一个空的win32控制台项目.名字为SimpleXyWrite，这个项目用来建立netcdf文件<br/>
⑶把如下文件拷贝到项目目录中<br/>
①netcdf源代码中的libsrc/netcdf.h头文件<br/>
②netcdf源代码中的examples/C/simple_xy_wr.c文件，并改名为simple_xy_wr.cpp<br/>
③netcdf预编译包中的netcdf.dll文件和 netcdf.lib文件<br/>
⑷把netcdf.h文件和simple_xy_wr.cpp加入到项目的文件列表中（具体菜单操作project->add to project->files）<br/>
⑸把netcdf.lib加入到项目的lib列表中（具体菜单操作project->add to project->settings->Link->object/library modules）<br/>
⑹编译并运行这个项目，会在项目目录下生成一个simple_xy.nc文件，其内容和我们手工生成的文件内容完全一样。

simple_xy_wr.c文件是建立netcdf文件的c代码，而examples/C/simple_xy_rd.c文件则是分析netcdf文件的代码，读者可以用和刚才类似的步骤在vc6中编译这个文件。运行时把把刚才生成的simple_xy.nc拷贝到项目的目录下，如果文件格式没错误，会提示*** SUCCESS reading example file simple_xy.nc!然后退出。

3、用ncgen命令自动生成c代码
给定了simple_xy.cdl文件后，可以用ncgen -c simple_xy.cdl命令直接在屏幕上输出c代码.不过，这个办法只限于cdl的数据比较简单时才可以采用。对于真正的项目，是需要我们自己去编写代码的。

# 小结

通过以上内容的介绍，相信读者对netcdf已经有了大致的了解。总体来说，netcdf的核心内容就是通过cdl描述文法来建立一个netcdf格式文件。抓住了这一点，读者再继续深入看netcdf的其他资料时，应该就没什么太大的难度了。如是，作者的目的也就达到了。

# 附录

为便于读者对照，现本文中用到的几个文件的内容在此列出<br/>
1、simple_xy.cdl文件的内容<br/>

```
netcdf simple_xy {
dimensions:
x = 6 ;
y = 12 ;
variables:
int data(x, y) ;
data:
data =
0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71 ;
}
```

2、simple_xy-wr.c文件的内容<br/>

```
/* This is part of the netCDF package.
Copyright 2006 University Corporation for Atmospheric Research/Unidata.
See COPYRIGHT file for conditions of use.

This is a very simple example which writes a 2D array of
sample data. To handle this in netCDF we create two shared
dimensions, "x" and "y", and a netCDF variable, called "data".

This example demonstrates the netCDF C API. This is part of the
netCDF tutorial, which can be found at:
http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial

Full documentation of the netCDF C API can be found at:
http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-c

$Id: simple_xy_wr.c,v 1.12 2007/02/14 20:59:21 ed Exp $
*/
#include <stdlib.h>
#include <stdio.h>
#include <netcdf.h>

/* This is the name of the data file we will create. */
#define FILE_NAME "simple_xy.nc"

/* We are writing 2D data, a 6 x 12 grid. */
#define NDIMS 2
#define NX 6
#define NY 12

/* Handle errors by printing an error message and exiting with a
* non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s/n", nc_strerror(e)); exit(ERRCODE);}

int
main()
{
/* When we create netCDF variables and dimensions, we get back an
* ID for each one. */
int ncid, x_dimid, y_dimid, varid;
int dimids[NDIMS];

/* This is the data array we will write. It will be filled with a
* progression of numbers for this example. */
int data_out[NX][NY];

/* Loop indexes, and error handling. */
int x, y, retval;

/* Create some pretend data. If this wasn't an example program, we
* would have some real data to write, for example, model
* output. */
for (x = 0; x < NX; x++)
for (y = 0; y < NY; y++)
data_out[x][y] = x * NY + y;

/* Always check the return code of every netCDF function call. In
* this example program, any retval which is not equal to NC_NOERR
* (0) will cause the program to print an error message and exit
* with a non-zero return code. */

/* Create the file. The NC_CLOBBER parameter tells netCDF to
* overwrite this file, if it already exists.*/
if ((retval = nc_create(FILE_NAME, NC_CLOBBER, &ncid)))
ERR(retval);

/* Define the dimensions. NetCDF will hand back an ID for each. */
if ((retval = nc_def_dim(ncid, "x", NX, &x_dimid)))
ERR(retval);
if ((retval = nc_def_dim(ncid, "y", NY, &y_dimid)))
ERR(retval);

/* The dimids array is used to pass the IDs of the dimensions of
* the variable. */
dimids[0] = x_dimid;
dimids[1] = y_dimid;

/* Define the variable. The type of the variable in this case is
* NC_INT (4-byte integer). */
if ((retval = nc_def_var(ncid, "data", NC_INT, NDIMS,
dimids, &varid)))
ERR(retval);

/* End define mode. This tells netCDF we are done defining
* metadata. */
if ((retval = nc_enddef(ncid)))
ERR(retval);

/* Write the pretend data to the file. Although netCDF supports
* reading and writing subsets of data, in this case we write all
* the data in one operation. */
if ((retval = nc_put_var_int(ncid, varid, &data_out[0][0])))
ERR(retval);

/* Close the file. This frees up any internal netCDF resources
* associated with the file, and flushes any buffers. */
if ((retval = nc_close(ncid)))
ERR(retval);

printf("*** SUCCESS writing example file simple_xy.nc!/n");
return 0;
}
```

3、simple_xy_rd.c文件的内容<br/>

```
/* This is part of the netCDF package.
Copyright 2006 University Corporation for Atmospheric Research/Unidata.
See COPYRIGHT file for conditions of use.

This is a simple example which reads a small dummy array, which was
written by simple_xy_wr.c. This is intended to illustrate the use
of the netCDF C API.

This program is part of the netCDF tutorial:
http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial

Full documentation of the netCDF C API can be found at:
http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-c

$Id: simple_xy_rd.c,v 1.9 2006/08/17 23:00:55 russ Exp $
*/
#include <stdlib.h>
#include <stdio.h>
#include <netcdf.h>

/* This is the name of the data file we will read. */
#define FILE_NAME "simple_xy.nc"

/* We are reading 2D data, a 6 x 12 grid. */
#define NX 6
#define NY 12

/* Handle errors by printing an error message and exiting with a
* non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s/n", nc_strerror(e)); exit(ERRCODE);}

int
main()
{
/* This will be the netCDF ID for the file and data variable. */
int ncid, varid;

int data_in[NX][NY];

/* Loop indexes, and error handling. */
int x, y, retval;

/* Open the file. NC_NOWRITE tells netCDF we want read-only access
* to the file.*/
if ((retval = nc_open(FILE_NAME, NC_NOWRITE, &ncid)))
ERR(retval);

/* Get the varid of the data variable, based on its name. */
if ((retval = nc_inq_varid(ncid, "data", &varid)))
ERR(retval);

/* Read the data. */
if ((retval = nc_get_var_int(ncid, varid, &data_in[0][0])))
ERR(retval);

/* Check the data. */
for (x = 0; x < NX; x++)
for (y = 0; y < NY; y++)
if (data_in[x][y] != x * NY + y)
return ERRCODE;

/* Close the file, freeing all resources. */
if ((retval = nc_close(ncid)))
ERR(retval);

printf("*** SUCCESS reading example file %s!/n", FILE_NAME);
return 0;
}
```
