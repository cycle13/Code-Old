---
layout:     post
title:      "/usr/bin/ld: cannot find -l* 错误的解决方法"
subtitle:   "Cannot find -l* errors and solutions"
date:       2017-05-30
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Linux系统运维与服务器管理
    - 我的笔记
---

# 原因：

1.系统缺乏对应的库文件；

2.版本不对应；

3.库文件的链接错误；

4.库文件路径设置问题

# 方法一：

可用`yum install libtool-l*`来安装`yum -y install libtool-l*`

譬如：`/usr/bin/ld: cannot find -lltdl`

解决方法：`yum -y install libtool-ltdl*`

# 方法二：

当方法一不能成功时，可从其他机器拷贝文件到本机`/usr/lib64/`。具体文件：`/usr/lib64/`目录下与`lib*`相关文件（主要是二个：`lib*.a`和`lib*.so`），最好是将`lib*.a`和`lib*.so`都链接到`/usr/lib64`目录下。

# 方法三:

通过`find`或者`locate`指令定位到链接文件，查看链接文件是否正确的指向了我们希望的lib，如果不是，用`ln -sf */libxxx.so.x */libxxx.so`指令修改它。

# 方法四：

如果是库文件路径引发的问题，可以到`/etc/ld.so.conf.d`目录下，修改其中任意一份conf文件，（可以自建conf，以方便识别）将lib所在目录写进去，然后在终端输入 ldconfig 更新缓存。