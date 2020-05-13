---
layout:     post
title:      "Header Bidding"
subtitle:   "Header Bidding"
date:       2017-07-12
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - 产品经理
    - 我的笔记
---

> 最近杂志看到一些关于header bidding的介绍，这里做个简单的总结。Header Bidding是一种新的程序化交易广告技术。Header Bidding在2016年由AppNexus最早提出。

# 传统的RTB竞价方式

假设你是一个广告主，比如你是Adidas，希望放一些广告出去推广自己的产品，目标客户是大学生。在这一一个过程中，购买能够展示广告的广告位是一个重要的过程。

![img](/img/in-post/2017-07-12-header-bidding/01.png)

这张图展示了一般情况下广告主是如何获得广告位的。我们对图片做一些解释。

广告主首先会找到与自己有合作关系的媒体方，然后把广告放在这些媒体上面，这是最直接投广告的方式。

此外，广告主也会把自己的需求（即想购买广告位）提交给广告交易平台的需求方平台（Demand-Side platform，DSP）。DSP会和数据管理平台（Data-Management platform，DMP）相互沟通，DMP提供给DSP一些关于目标用户的信息，选择不同媒体背后的人群属性（例如大学男生可能比较喜欢看新浪运动频道之类的）。广告交易平台上面的供给方平台（Supply-Side platform，SSP）搜集了媒体方的广告位。此时，SSP会向DSP发起RTB询价请求，然后找到广告交易平台认为最佳的配置，然后直接把RTB得到的广告（注意这里要强调媒体方直接得到的是广告，而不是广告对应的价格）直接交给媒体方，然后媒体方进行展示。RTB会把竞价者出的价格按照某种方式做一个排序，逐次确认竞价者是否愿意花钱买这个广告位。如果没有人愿意买，那么这个广告位最后就会落入地板价（floor）。

这种方式存在的问题主要有如下两个方面：一方面是广告交易平台（如Google，Mopub）往往会偏爱自己内部的广告主；另一方面是对媒体方收取的费用比较高。这两个问题损害了广告主和媒体方两方的利益。

因此，header bidding应运而生。

# Header Bidding的机制

我们来看header bidding是怎么避免RTB造成的两个问题的。

![img](/img/in-post/2017-07-12-header-bidding/02.png)

这张图概括了传统RTB竞价与header bidding方式的主要内容。从图中可以看到，传统的RTB竞价，实际上就是媒体方向广告交易平台请求广告，广告交易平台发起竞价，然后按照一种内部的算法，把广告直接返回给媒体方。而Header Bidding的机制是这样：媒体方首先先通过其客户端，直接向若干个合作伙伴（可以是有合作关系的广告主，可以是DSP等等）请求广告，然后得到这些合作伙伴们返回的出价。这样，媒体方相当于对自己所拥有的这一广告位的价值有了一定的认识。然后，媒体方可以选择是否要通过广告交易平台发起RTB。广告交易平台发起RTB媒体方最后只能得到广告，这样就没法和Header Bidding得到的出价直接相比较。这时候有两种办法：1.和广告交易平台协商，让他们提供RTB得到的广告的出价；2.把Header Bidding得到的最高出价作为地板价发给广告交易平台开始RTB。总之，就是媒体方可以主动选择，是采用Header Bidding方式得到的广告还是使用广告交易平台提供的广告。

实际上，header bidding可以从形式上理解为一种加强的RTB。当用户访问媒体网站之后，媒体网站首先进行Header Bidding，然后进行RTB，将两次竞价结果综合到一起，价高者胜。

从这些过程中我们也可以注意到，传统的广告交易平台，RTB的发起者一般是SSP，而Header Bidding的发起者则是客户端。这样，主动权就从广告交易平台转移到了媒体方。Header Bidding的存在，对媒体方而言，提高了竞价密度，有助于提升CPM；而对于广告主而言，有了新渠道，有助于提升ROI。双方联手打破广告交易平台的垄断，降低了渠道费用。

当然，Header Bidding的存在并不十全十美，它也有一些问题。最核心的问题是，客户端每次发动Header Bidding会导致客户端响应的延迟，这种高延迟响应会极大地影响用户浏览这个网站的兴趣，媒体方的曝光量减少，导致愿意来投放广告的广告主的数量减少。因此，Header Bidding能否真正打破广告交易平台的垄断还有待观察。这也提醒我们，广告技术本质上是为商业服务的，商业上是否有新的价值产生，远比技术上的进步和合理更加重要。

# Header Bidding的技术方案

顾名思义，Header Bidding就是在Head发动竞价。实际上，就是在HTML的head嵌入一些JavaScript SDK，从而使得网页加载时能够获取多个竞价者的报价。然后再通过广告交易平台获取广告，最后才是生成投放的广告。下面给出一段[Prebid.js](http://prebid.org/)提供的范例程序。


```
<html>

<body style="margin:0;padding:0">

<script>

    // Define the sizes here:
    var adUnitSizes = [[300,250]];

    // Add your post-bid tag IDs here:
    var adUnitBids = [
        {
            bidder: 'appnexus',
            params: {
               placementId: '10433394'
            }
        },
        {
            bidder: 'TO ADD',
            params: {
                publisherId: 'TO ADD',
                adSlot: 'TO ADD'
            }
        }
    ]

    // Define the passback HTML tag here. Note that this tag is usually in either Script tag form or iFrame form. For example:

    /* iFrame:
        var passbackTagHtml = '<iframe src="http://ad.doubleclick.net/...;sz=300x250;ord=[timestamp]?" width="300" height="250" marginwidth="0" marginheight="0" hspace="0" vspace="0" frameborder="0" scrolling="no"></iframe>'
    */

    /* Script:
        var passbackTagHtml = '<scr' + 'ipt type='text/javascript' src='https://www.googletagservices.com/tag/js/gpt.js'>googletag.pubads().definePassback('/19968/header-bid-passback-tag', [300, 250]).display();</scr' + 'ipt>'
    */

    var passbackTagHtml = 'TO ADD';



    // ======= DO NOT EDIT BELOW THIS LINE =========== //
    
    var pbjs = pbjs || {};
    pbjs.que = pbjs.que || [];

    (function() {
        var pbjsEl = document.createElement("script"); pbjsEl.type = "text/javascript";
        pbjsEl.async = true; var isHttps = 'https:' === document.location.protocol;
        pbjsEl.src = "//acdn.adnxs.com/prebid/not-for-prod/prebid.js";
        var pbjsTargetEl = document.getElementsByTagName("head")[0];
        pbjsTargetEl.insertBefore(pbjsEl, pbjsTargetEl.firstChild);
    })();

    var doNotChange = 'doNotChange';

    pbjs.que.push(function() {

        var adUnits = [{
            code: doNotChange,
            sizes: adUnitSizes,
            bids: adUnitBids
        }];

        pbjs.addAdUnits(adUnits);

        pbjs.requestBids({
            timeout: 800,
            bidsBackHandler: function() {
                var iframe = document.getElementById('postbid_if');
                
                var iframeDoc = iframe.contentWindow.document;

                var params = pbjs.getAdserverTargetingForAdUnitCode(doNotChange);

                // If any bidders return any creatives
                if(params && params['hb_adid']){
                    pbjs.renderAd(iframeDoc, params['hb_adid']);
                } else {
                    // If no bidder return any creatives,
                    // Passback 3rd party tag in Javascript

                    iframe.width = adUnitSizes[0][0];
                    iframe.height = adUnitSizes[0][1];

                    iframeDoc.write(passbackTagHtml);
                }
            }
        });
    });  

</script>

<iframe id='postbid_if' FRAMEBORDER="0" SCROLLING="no" MARGINHEIGHT="0" MARGINWIDTH="0" TOPMARGIN="0" LEFTMARGIN="0" ALLOWTRANSPARENCY="true" WIDTH="0" HEIGHT="0"></iframe>

</body>

</html>
```