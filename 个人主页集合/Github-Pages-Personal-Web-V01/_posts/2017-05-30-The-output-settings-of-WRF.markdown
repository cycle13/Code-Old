---
layout:     post
title:      "WRF模式变量输出设置"
subtitle:   "The output settings of WRF"
date:       2017-05-30
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - 天气预报与数值模式
---

> WRF输出多少变量有两种方式：一种是参考WRF文件夹下的README.io_config，可以把Registry里的变量输出到wrfout文件中；另一种是逐层传递变量，最终在Registry里增加一些变量。

# 修改io_config

通过修改io_config，可以增加输出一些Registry里已经声明好的变量。修改方法如下：

1.在namelist.input里修改iofields_filename这条参数，比如令其为"myfile"；

2.生成一个myfile，里面的格式如：op:streamtype:streamid:variables；

op: +(增加)，-(移除)

streamtype: i(输入资料)，h(历史输出)

streamid: 一个整数，0代表主输出，即wrfout

variables: 用逗号分隔的变量列表，变量名具体见Registry

例如：-:h:0:SEAICE，在wrfout里不输出SEAICE这个变量；+:i:5:u，在5号输出文件里输出u变量

# 修改Registry

内部变量主要通过 Registry来控制（WRF/Registry ）。与real run相对应的是 Registry.EM。

该文件每一行分为 9列：

Type   Sym   Dims   Use   Tlev   Stag   IO   Dname   Descrip 

每个变量对应一列，比如： 

state   real   u   ikjb   dyn_em   2   X   i01rhusdf   "U"   "X WIND COMPONENT" 

这一行就对应 WRF中所用的U变量。 

控制输出的在于 IO一栏（i01rhusdf ），i:输入。r:重启 (restart)输出。如果在IO栏中有 "r"，则该变量就会输出到wrfrst\*文件中，以便于未来 restart run(hot start)时作为初始条件使用。h：输出，即输出到 history文件中。usdf：控制 nesting选项。 


如果删除 h, 则U变量将不会输出到 history文件中（wrfout\* ），也不会有任何的辅助输出。

如果改为 h1, 则U 变量将不会输出到 history主文件中（wrfout\* ）, 但会输出到辅助输出文件 1中。

如果改为 h01，则U 变量既会输出到 history主文件中（wrfout\* ），同时也会输出到辅助输出文件 1中。

h之后可以跟 0(主输出) ，也可以跟 "1,2,3,4,5,6,7,8,9",对应1 到9号辅助输出文件。以下写法，都是合法的：h, h01, h012, h347。


在Registry中修改rainc和rainnc输出为 h02后，要在run/namelist.input 中修改添加以下变量：

auxhist2_outname = "rainfall_d<domain>_<date>"--辅助文件2 的命名

auxhist2_interval = 60--变量输出的间隔，以分钟为单位 

io_form_auxhist2 = 2--辅助文件 2的输出格式，2对应netcdf 

frame_per_auxhist2 = 1--每个文件中包含的输出数 

如果想要修改输出频率，可以在这里更改 auxhist2_interval注意： 

如果没有添加这些变量在namelist.input中，辅助输出文件（rainfall\*）还是不能生成。

当然，在上面这些基础上，要把目标变量从WRF每个模块里传出来，一直传到动力框架，WRF的grid变量里。