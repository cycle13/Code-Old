---
layout:     post
title:      "Python simplejson模块"
subtitle:   "Simplejson module of python"
date:       2017-08-24
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Python
---

python安装：easy_install simplejson

导入模块：


`import simplejson as json`

几个主要函数：dump,dumps,load,loads 

带s跟不带s的区别是 带s的是对字符串的处理，而不带s的是对文件对像的处理。

json化python字典数据：

```
json.dumps(['foo', {'bar': ('baz', None, 1.0, 2)}])

'["foo", {"bar": ["baz", null, 1.0, 2]}]'

print json.dumps("\"foo\bar")
"\"foo\bar"
```

sort_keys设置是否排序字典：

```
print json.dumps({"c": 0, "b": 0, "a": 0}, sort_keys=True)
{"a": 0, "b": 0, "c": 0}
```

创建文件流对象：

```
from StringIO import StringIO
io = StringIO()
```

把 json编码数据导向到此文件对象：

```
json.dump(['streaming API'], io)
```

取得文件流对象的内容：

```
io.getvalue()
'["streaming API"]'
```

压缩编码：

```
import simplejson as json
json.dumps([1,2,3,{'4': 5, '6': 7}], separators=(',',':'))
'[1,2,3,{"4":5,"6":7}]'
```

美化打印：

```
import simplejson as json
s = json.dumps({'4': 5, '6': 7}, sort_keys=True, indent=' ')
print '\n'.join([l.rstrip() for l in s.splitlines()])
{
"4": 5,
"6": 7
}
```

解析json字符串：

```
obj = [u'foo', {u'bar': [u'baz', None, 1.0, 2]}]
json.loads('["foo", {"bar":["baz", null, 1.0, 2]}]') == obj
True
json.loads('"\\"foo\\bar"') == u'"foo\x08ar'
True
from StringIO import StringIO
io = StringIO('["streaming API"]')
json.load(io)[0] == 'streaming API'
True
```

指定json解析后的对象：

```
import simplejson as json
def as_complex(dct):
if '__complex__' in dct:
return complex(dct['real'], dct['imag'])
return dct

json.loads('{"__complex__": true, "real": 1, "imag": 2}',
object_hook=as_complex)
(1+2j)
from decimal import Decimal
json.loads('1.1', parse_float=Decimal) == Decimal('1.1')
True
```

指定编码对象的格式：

```
import simplejson as json
def encode_complex(obj):
if isinstance(obj, complex):
return [obj.real, obj.imag]
raise TypeError(repr(o) + " is not JSON serializable")

json.dumps(2 + 1j, default=encode_complex)
'[2.0, 1.0]'
json.JSONEncoder(default=encode_complex).encode(2 + 1j)
'[2.0, 1.0]'
''.join(json.JSONEncoder(default=encode_complex).iterencode(2 + 1j))
'[2.0, 1.0]'
```