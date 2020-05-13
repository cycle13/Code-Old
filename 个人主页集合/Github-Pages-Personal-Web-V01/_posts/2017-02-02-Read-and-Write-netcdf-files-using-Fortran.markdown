---
layout:     post
title:      "Fortran读写netcdf格式文件"
subtitle:   "Read and Write netcdf files using Fortran"
date:       2017-02-02
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Fortran
    - NetCDF
    - 我的笔记
---

在`implicit none`前一行使用`use netcdf`，ifort编译命令为`ifort ***.f90 -I$NETCDF_INC -L$NETCDF_LIB -lnetcdff -lnetcdf`，主要是要连接netcdf库

这是使用F90格式读写<br/>
读取的部分命令：<br/>
ncfile=NF90_OPEN(filepath,nf90_nowrite,ncid)<br/>
ncfile=NF90_INQ_VARID(ncid,varname,varid)<br/>
ncfile=NF90_GET_VAR(ncid,varid,Variable)<br/>
ncfile=NF90_CLOSE(ncid)<br/>
也有读取属性的命令，详见官方手册<br/>
需要注意的是，Variable数组的各维大小必须和文件中大小相同，如果大于文件，多余默认为0<br/>
输出需要creat一个netcdf格式文件，也可以写入已有文件，需要注意的是，输出相对较麻烦，需要定义各个维度。