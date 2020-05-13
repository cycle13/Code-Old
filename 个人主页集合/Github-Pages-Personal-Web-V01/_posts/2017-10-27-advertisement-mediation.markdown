---
layout:     post
title:      "移动广告Mediation简单介绍"
subtitle:   "Mediation"
date:       2017-10-27
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - 产品经理
---

目前移动广告盛行，各家各户都想着打造属于自己的移动广告平台，都想着成为国内移动广告行业的Admob和Mopub. 也因此衍生出很多的名词，程序化购买，大数据，SSP，DSP...

各家都做属于自己的东西，都做自己的Ad Network，这也因此出现了别的机会，什么Exchange，RTBD等等，当然这个是在Demand端的，服务广告主，另一端的就是流量端，需要服务于开发者，各家各户都有属于自己的SDK,API供Publisher接入，然后，中间商又出现了，mediation，我把你们家，你们家，你们家的sdk放在一起，哎，好了，中间商我的新的SDK又出现了，就是这样。

Mediation有三种模式：

* WaterFall

* Fan Out

* Hybrid

WaterFall就是首先去访问SDK1,如果SDK1返回了广告，就Serve给Publisher；如果SDK1没有广告返回，就访问SDK2. 依次访问，直到有广告Serve给Publisher就完毕。

Fan Out既是一次请求所有的SDK，谁家SDK最短时间返回广告就Serve这家的广告到Publisher。

Hybrid就是一种混合模式了，一次全部SDK请求，返回的各家SDK广告对比最好的eCPM, 谁家eCPM高就Serve谁家的广告。

Mediation的赚钱点就是抽取一定的费用或者把自家的offer晒进去。你家都能Mediation，那我家当然也能Mediation啊，好家伙，Mediation也多起来了，热闹的移动互联网。这就比各家的重点了，是服务端Mediation呢还是客户端Mediation？是平台支持呢还是游戏引擎支持呢？还有就是各家是视频集成牛逼呢还是Banner，插屏牛逼呢？还要看是不是有好的用户数据借口呢？是否能针对性的优化呢？。。。

各有千秋。
