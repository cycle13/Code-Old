---
layout:     post
title:      "用Python制作一个简单的API"
subtitle:   "Create a simple API using Python"
date:       2017-02-22
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
    - Web开发
    - 我的笔记
---

> API(Application Programming Interface)是一种操作系统或者程序的接口，属于用户接口，主要为用户方便地提供各种服务。API的实现看似很简单，其实深入挖掘以后，有很多值得学习和处理的地方。下面我们进行一些简单的讨论。

# Flask API

[Flask API](http://www.flaskapi.org/)是基于Flask的一个API框架，它的基本结构主要都是继承自[Django REST framework](http://www.django-rest-framework.org/)。当然，实际情况是不用Flask API也是可以基于Flask来提供API服务的。但是Flask API毕竟已经把东西都弄好了，能不造轮子就不造轮子了。下面就是一个非常简化的API，它接收一个GET请求，然后返回一个json格式的数据，提供给用户。我们也可以看到，这段API代码的风格结构和Flask几乎是一模一样。


```
from flask import jsonify
from flask_api import FlaskAPI

application = FlaskAPI(__name__)
route = application.route
application.debug = True

@route("/<latlon>", methods=['GET'])
def index(latlon):
    return jsonify({'latlon'}: latlon)
    
if __name__ == "__main__":
    application.run()
```


# 测试

API主要是对用户提供服务，所以最重要的一点就是响应速度。这里介绍一些简单的测试办法，用来测试API的性能。当然，这里介绍的只是测试API速度的工具，至于怎么优化程序，让API响应更快，服务更稳定，这就不可避免的牵涉到算法与数据结构的问题了。

## curl

手工测试API的性能可以使用curl，然后观察返回的数据是否正确。命令如下`curl -X GET http://127.0.0.1:5000/32,119`，其中`127.0.0.1:5000`是本机的IP。

## ab

ab就是Apache HTTP server benchmarking tool，可以从来测试API的响应速度，命令如下：`ab -n 10000 http://127.0.0.1:5000/32,119`，这条命令将会对`127.0.0.1:5000`请求一万次，然后返回详细的情况给程序员分析参考。其中最直接的结果就是Time per request这个参数，可以根据这个参数得出平均情况下每条请求需要耗时多少。

