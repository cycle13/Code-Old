---
layout:     post
title:      "在shell脚本中执行shell脚本"
subtitle:   "Shell in Shell"
date:       2017-11-12
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Linux系统运维与服务器管理
    - 我的笔记
---

其实在shell脚本中执行shell脚本本身不是一件很复杂的事情，只有一种比较特殊的情况需要单独考虑：如果shell脚本在执行时需要在命令行输入一些数字或者字符，那么这个脚本在另一个shell脚本中执行的时候怎么办呢？

笔者在做PM时就遇到这个问题，如果有大量的配置文件需要上传到服务器，而上传脚本每次只能传一个文件。

于是琢磨出一种很取巧的办法，请看代码，非常简单：

```
#!/bin/sh

read -p "1、debug; 2、sandbox；3、production；0、test：" index

for myjson in `ls ./temp/configs/*.json`
do
	(echo ${index}
	sleep 1
        echo 4
	sleep 1
	echo ${myjson##*/}
	) | ./upload.sh
	echo "===================${myjson##*/} is uploaded==================="
done
```
