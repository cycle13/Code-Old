---
layout:     post
title:      "Python中时间日期相关的模块"
subtitle:   "Date and time modules in Python"
date:       2017-02-02
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

# time模块

## 壁挂钟时间

### time()

time模块的核心函数time()，它返回纪元开始的秒数，返回值为浮点数，具体精度依赖于平台。

```
import time
time.time()
```

输出`1486016604.351411`

### ctime()

浮点数一般用于存储和比较日期，要记录和打印时间，可以使用ctime()。

```
import time
time.ctime()
```

输出`'Thu Feb  2 14:25:11 2017'`

```
later = time.time()+5
time.ctime(later)
```

输出`'Thu Feb  2 14:25:59 2017'`

## 处理器时钟时间

clock()返回处理器时钟时间，它的返回值一般用于性能测试与基准测试。因此它们反映了程序的实际运行时间。

```
import time
time.clock()
```

输出`0.026528`

## 时间组成

time模块定义了struct_time来维护时间和日期，其中分开存储各个组成部分，以便访问。gmtime()用于获取UTC时间，localtime()用于获取当前时区的当前时间，UTC时间实际就是格林尼治时间，它与中国时间的时差为八个小时。

```
import time
time.gmtime()
```

输出`time.struct_time(tm_year=2017, tm_mon=2, tm_mday=2, tm_hour=6, tm_min=29, tm_sec=6, tm_wday=3, tm_yday=33, tm_isdst=0)`

```
import time
time.localtime()
```

输出`time.struct_time(tm_year=2017, tm_mon=2, tm_mday=2, tm_hour=14, tm_min=31, tm_sec=41, tm_wday=3, tm_yday=33, tm_isdst=0)`

```
time.localtime().tm_year
```

输出2017

## 处理时区

### 获取时间差

```
import time
time.timezone/3600
```

输出`-8`

### 设置时区

```
ZONES = ["GMT", "EUROPE/Amsterdam']
for zone in ZONES:
    os.environ["TZ"] = zone
    time.tzset()
```

## 解析和格式化时间

> time模块提供了两个函数strptime（）和strftime（），可以在struct_time和时间值字符串之间转换。

### strptime()

用于将字符串时间转换成struct_time格式：

```
now=time.ctime()
time.strptime(now)
```

输出`time.struct_time(tm_year=2017, tm_mon=2, tm_mday=2, tm_hour=14, tm_min=38, tm_sec=42, tm_wday=3, tm_yday=33, tm_isdst=-1)`

### strftime()

用于时间的格式化输出

```
from time import gmtime, strftime
strftime("%a, %d %b %Y %H:%M:%S +0000", gmtime())
```

输出`'Thu, 02 Feb 2017 06:42:20 +0000'`

### mktime()

用于将struct_time转换成时间的浮点数表示

```
from time import mktime, gmtime
mktime(gmtime())
```

输出`1485989006.0`

### asctime()

接受时间元组并返回一个可读的形式为Weekday Month Day HH:MM:SS Year的字符。

```
time.asctime(time.localtime())
```

输出`'Thu Feb  2 14:46:37 2017'`

### 格式化字符串的含义

![img](/img/in-post/2017-02-02-Date-and-time-modules-in-Python/01.png)

## sleep()

sleep函数用于将当前线程交出，要求它等待系统将其再次唤醒，如果写程序只有一个线程，这实际上就会阻塞进程，什么也不做。

```
import time
time.sleep(5)
print "hello world!"
```

这段代码将会在5秒后输出hello world！

# datetime模块

> datetime模块定义了两个常量：datetime.MINYEAR和datetime.MAXYEAR，分别表示datetime所能表示的最小、最大年份。其中，MINYEAR = 1，MAXYEAR = 9999。datetime.date：表示日期的类。常用的属性有year, month, day；datetime.time：表示时间的类。常用的属性有hour, minute, second, microsecond；datetime.datetime：表示日期时间。datetime.timedelta：表示时间间隔，即两个时间点之间的长度。datetime.tzinfo：与时区有关的相关信息。

## date类

date类表示一个日期。日期由年、月、日组成。date类的构造函数如下：

```
datetime.date(year, month, day)
```

year的范围是[MINYEAR, MAXYEAR]，即[1, 9999]；month的范围是[1, 12]。day的最大值根据给定的year, month参数来决定。例如闰年2月份有29天。

date类提供了一些类方法与类属性：<br/>
date.max、date.min：date对象所能表示的最大、最小日期<br/>
date.resolution：date对象表示日期的最小单位，这里是天<br/>
date.today()：返回一个表示当前本地日期的date对象<br/>
date.fromtimestamp(timestamp)：根据给定的时间戮，返回一个date对象<br/>
datetime.fromordinal(ordinal)：将Gregorian日历时间转换为date对象

date提供的实例方法和属性：<br/>
date.year、date.month、date.day：年、月、日<br/>
date.replace(year, month, day)：生成一个新的日期对象，用参数指定的年，月，日代替原有对象中的属性（原有对象仍保持不变）<br/>
date.timetuple()：返回日期对应的time.struct_time对象<br/>
date.toordinal()：返回日期对应的Gregorian Calendar日期<br/>
date.weekday()：返回weekday，如果是星期一，返回0；如果是星期2，返回1，以此类推<br/>
data.isoweekday()：返回weekday，如果是星期一，返回1；如果是星期2，返回2，以此类推<br/>
date.isocalendar()：返回格式如(year，month，day)的元组<br/>
date.isoformat()：返回格式如'YYYY-MM-DD’的字符串<br/>
date.strftime(fmt)：自定义格式化字符串

date还对某些操作进行了重载，它允许我们对日期进行如下一些操作：<br/>
date2 = date1 + timedelta # 日期加上一个间隔，返回一个新的日期对象（timedelta将在下面介绍，表示时间间隔）<br/>
date2 = date1 - timedelta # 日期隔去间隔，返回一个新的日期对象<br/>
timedelta = date1 - date2 # 两个日期相减，返回一个时间间隔对象<br/>
date1 < date2 # 两个日期进行比较

## time类

time类表示时间，由时、分、秒以及微秒组成。time类的构造函数如下：

```
class datetime.time(hour[ , minute[ , second[ , microsecond[ , tzinfo]]]] )
```

time类定义的类属性：<br/>
time.min、time.max：time类所能表示的最小、最大时间。其中，time.min = time(0, 0, 0, 0)， time.max = time(23, 59, 59, 999999)；<br/>
time.resolution：时间的最小单位，这里是1微秒；

time类提供的实例方法和属性：<br/>
time.hour、time.minute、time.second、time.microsecond：时、分、秒、微秒；<br/>
time.tzinfo：时区信息；<br/>
time.replace([ hour[ , minute[ , second[ , microsecond[ , tzinfo]]]]] )：创建一个新的时间对象，用参数指定的时、分、秒、微秒代替原有对象中的属性（原有对象仍保持不变）；<br/>
time.isoformat()：返回型如"HH:MM:SS"格式的字符串表示；<br/>
time.strftime(fmt)：返回自定义格式化字符串。

## datetime类

datetime是date与time的结合体，包括date与time的所有信息。它的构造函数如下：

```
datetime.datetime (year, month, day[ , hour[ , minute[ , second[ , microsecond[ , tzinfo]]]]] )
```

datetime类定义的类属性与方法：<br/>
datetime.min、datetime.max：datetime所能表示的最小值与最大值；<br/>
datetime.resolution：datetime最小单位；<br/>
datetime.today()：返回一个表示当前本地时间的datetime对象；<br/>
datetime.now([tz])：返回一个表示当前本地时间的datetime对象，如果提供了参数tz，则获取tz参数所指时区的本地时间；<br/>
datetime.utcnow()：返回一个当前utc时间的datetime对象；<br/>
datetime.fromtimestamp(timestamp[, tz])：根据时间戮创建一个datetime对象，参数tz指定时区信息；<br/>
datetime.utcfromtimestamp(timestamp)：根据时间戮创建一个datetime对象；<br/>
datetime.combine(date, time)：根据date和time，创建一个datetime对象；<br/>
datetime.strptime(date_string, format)：将格式字符串转换为datetime对象；

datetime类提供的实例方法与属性：<br/>
datetime.year、month、day、hour、minute、second、microsecond、tzinfo<br/>
datetime.date()：获取date对象；<br/>
datetime.time()：获取time对象；<br/>
datetime.replace ([ year[ , month[ , day[ , hour[ , minute[ , second[ , microsecond[ , tzinfo]]]]]]]] )<br/>
datetime.timetuple()<br/>
datetime.utctimetuple()<br/>
datetime.toordinal()<br/>
datetime.weekday()<br/>
datetime.isocalendar()<br/>
datetime.isoformat ([ sep] )<br/>
datetime.ctime()：返回一个日期时间的C格式字符串，等效于time.ctime(time.mktime(dt.timetuple()))<br/>
datetime.strftime (format)

# calendar模块

calendar.calendar(year,w=2,l=1,c=6)：返回一个多行字符串格式的year年年历，3个月一行，间隔距离为c。 每日宽度间隔为w字符。每行长度为21* W+18+2* C。l是每星期行数。

calendar.firstweekday()：返回当前每周起始日期的设置。默认情况下，首次载入caendar模块时返回0，即星期一。

calendar.isleap(year)：是闰年返回True，否则为false。

calendar.leapdays(y1,y2)：返回在Y1，Y2两年之间的闰年总数。

calendar.month(year,month,w=2,l=1)：返回一个多行字符串格式的year年month月日历，两行标题，一周一行。每日宽度间隔为w字符。每行的长度为7* w+6。l是每星期的行数。

calendar.monthcalendar(year,month)：返回一个整数的单层嵌套列表。每个子列表装载代表一个星期的整数。Year年month月外的日期都设为0;范围内的日子都由该月第几日表示，从1开始。

calendar.monthrange(year,month)：返回两个整数。第一个是该月的星期几的日期码，第二个是该月的日期码。日从0（星期一）到6（星期日）;月从1到12。

calendar.prcal(year,w=2,l=1,c=6)：相当于 print calendar.calendar(year,w,l,c)。

calendar.prmonth(year,month,w=2,l=1)：相当于 print calendar.calendar（year，w，l，c）。

calendar.setfirstweekday(weekday)：设置每周的起始日期码。0（星期一）到6（星期日）。

calendar.timegm(tupletime)：和time.gmtime相反：接受一个时间元组形式，返回该时刻的时间辍（1970纪元后经过的浮点秒数）。

calendar.weekday(year,month,day)：返回给定日期的日期码。0（星期一）到6（星期日）。月份为 1（一月） 到 12（12月）。