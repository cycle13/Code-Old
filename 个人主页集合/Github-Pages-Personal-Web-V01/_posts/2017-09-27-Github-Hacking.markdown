---
layout:     post
title:      "Github Hacking"
subtitle:   "Github Hacking"
date:       2017-09-27
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Git
---

Github不仅能托管代码，还能对代码进行搜索，我们感受到了其便利的同时，也应该时刻注意，当你上传并公开你的代码时，一时大意，让某些敏感的配置信息文件等暴露于众。

让我们从第一个例子开始。当搜索`ssh password`关键字时，其中里面有这样一个有趣的结果：

![img](/img/in-post/2017-09-27-Github-Hacking/01.png)

好像是一个捷克教育科研网络的，账号密码写的这么简洁明了，于是登录上去看一看。

![img](/img/in-post/2017-09-27-Github-Hacking/02.png)

是不是还挺欢乐的，早就有无数人登陆过了，还有人留下文本善意提醒。这意味着什么，Github早已被盯上，也许下一个大事件会是某漏洞导致Github私有库代码大量泄漏。

# 当我们在Github上搜索时，我们到底能搜到什么

能搜到的东西很多，这里只是给个思路，具体怎么玩自己去尝试。

## 邮箱

比如说以`mail password`关键字搜索：

![img](/img/in-post/2017-09-27-Github-Hacking/03.png)

![img](/img/in-post/2017-09-27-Github-Hacking/04.png)

搜索很多邮箱的帐号密码，这里就不一一列举了。

如果说用`@qq.com`或者是`@gmail.com`等各种邮箱后缀为关键字进行搜索，你会发现某商户收集的客户qq信息：

![img](/img/in-post/2017-09-27-Github-Hacking/05.png)

## 各种账号密码

Github上能搜到的账号密码实在是太多了，筛选一下你会发现很多有意思的。比如说有微信公众平台帐号：

![img](/img/in-post/2017-09-27-Github-Hacking/06.png)

居然连Github的登陆帐号也放在上面。。

![img](/img/in-post/2017-09-27-Github-Hacking/07.png)

## 各种VIP

万万没想到啊，没想到Github上还有这等福利！

![img](/img/in-post/2017-09-27-Github-Hacking/08.png)

![img](/img/in-post/2017-09-27-Github-Hacking/09.png)

## 百度云

尽管大部分链接已经失效，但是好资源还是有的。

![img](/img/in-post/2017-09-27-Github-Hacking/10.png)

## 简历

没想到还有很多人把包含个人信息的如此重要的简历也放在了Github上。搜索相关关键字`resume`，`简历`，`学历`：

![img](/img/in-post/2017-09-27-Github-Hacking/11.png)

## 其它

比如说有时候我需要微信开放平台的应用AppID（太懒，不想申请），于是搜索关键字`WXApi registerApp`，出来很多：

![img](/img/in-post/2017-09-27-Github-Hacking/12.png)

总之，鉴于越来越多人开始使用Github（非码农，比如说科学家，作家，音乐制作人，会计等职业），你可以在Github搜的内容也越来越多，不仅仅是代码，简直什么都有，什么某人做的笔记啊，写的小说啊，自拍照啊，还有书籍，论文等，简直出乎意料。