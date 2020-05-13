---
layout:     post
title:      "Python 多核编程 mpi4py 实践"
subtitle:   "Python mpi4py"
date:       2017-10-17
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

# 一、概述

CPU从三十多年前的8086，到十年前的奔腾，再到当下的多核i7。一开始，以单核cpu的主频为目标，架构的改良和集成电路工艺的进步使得cpu的性能高速上升，单核cpu的主频从老爷车的MHz阶段一度接近4GHz高地。然而，也因为工艺和功耗等的限制，单核cpu遇到了人生的天花板，急需转换思维，以满足无止境的性能需求。多核cpu在此登上历史舞台。给你的老爷车多加两个引擎，让你有法拉利的感觉。现时代，连手机都到处叫嚣自己有4核8核处理器的时代，PC就更不用说了。

扯远了，anyway，对于俺们程序员来说，如何利用如此强大的引擎完成我们的任务才是我们要考虑的。随着大规模数据处理、大规模问题和复杂系统求解需求的增加，以前的单核编程已经有心无力了。如果程序一跑就得几个小时，甚至一天，想想都无法原谅自己。那如何让自己更快的过度到高大上的多核并行编程中去呢？哈哈，广大人民的力量！

目前工作中我所接触到的并行处理框架主要有MPI、OpenMP和MapReduce(Hadoop)三个（CUDA属于GPU并行编程，这里不提及）。MPI和Hadoop都可以在集群中运行，而OpenMP因为共享存储结构的关系，不能在集群上运行，只能单机。另外，MPI可以让数据保留在内存中，可以为节点间的通信和数据交互保存上下文，所以能执行迭代算法，而Hadoop却不具有这个特性。因此，需要迭代的机器学习算法大多使用MPI来实现。当然了，部分机器学习算法也是可以通过设计使用Hadoop来完成的。（浅见，如果错误，希望各位不吝指出，谢谢）。

本文主要介绍Python环境下MPI编程的实践基础。

# 二、MPI与mpi4py

MPI是Message Passing Interface的简称，也就是消息传递。消息传递指的是并行执行的各个进程具有自己独立的堆栈和代码段，作为互不相关的多个程序独立执行，进程之间的信息交互完全通过显示地调用通信函数来完成。

Mpi4py是构建在mpi之上的python库，使得python的数据结构可以在进程（或者多个cpu）之间进行传递。

## 2.1、MPI的工作方式

很简单，就是你启动了一组MPI进程，每个进程都是执行同样的代码！然后每个进程都有一个ID，也就是rank来标记我是谁。什么意思呢？假设一个CPU是你请的一个工人，共有10个工人。你有100块砖头要搬，然后很公平，让每个工人搬10块。这时候，你把任务写到一个任务卡里面，让10个工人都执行这个任务卡中的任务，也就是搬砖！这个任务卡中的“搬砖”就是你写的代码。然后10个CPU执行同一段代码。需要注意的是，代码里面的所有变量都是每个进程独有的，虽然名字相同。


例如，一个脚本test.py，里面包含以下代码：


```
from mpi4py import MPI
print("hello world'')
print("my rank is: %d" %MPI.rank)
```

然后我们在命令行通过以下方式运行：

```
#mpirun –np 5 python test.py
```

-np5 指定启动5个mpi进程来执行后面的程序。相当于对脚本拷贝了5份，每个进程运行一份，互不干扰。在运行的时候代码里面唯一的不同，就是各自的rank也就是ID不一样。所以这个代码就会打印5个hello world和5个不同的rank值，从0到4.

## 2.2、点对点通信

点对点通信（Point-to-PointCommunication）的能力是信息传递系统最基本的要求。意思就是让两个进程直接可以传输数据，也就是一个发送数据，另一个接收数据。接口就两个，send和recv，来个例子：

```
import mpi4py.MPI as MPI
comm = MPI.COMM_WORLD
comm_rank = comm.Get_rank()
comm_size = comm.Get_size()
# point to point communication
data_send = [comm_rank]*5
comm.send(data_send,dest=(comm_rank+1)%comm_size)
data_recv =comm.recv(source=(comm_rank-1)%comm_size)
print("my rank is %d, and Ireceived:" % comm_rank)
print data_recv
```

启动5个进程运行以上代码，结果如下：

```
my rank is 0, and I received:
[4, 4, 4, 4, 4]
my rank is 1, and I received:
[0, 0, 0, 0, 0]
my rank is 2, and I received:
[1, 1, 1, 1, 1]
my rank is 3, and I received:
[2, 2, 2, 2, 2]
my rank is 4, and I received:
[3, 3, 3, 3, 3]
```

可以看到，每个进程都创建了一个数组，然后把它传递给下一个进程，最后的那个进程传递给第一个进程。comm\_size就是mpi的进程个数，也就是-np指定的那个数。MPI.COMM\_WORLD 表示进程所在的通信组。

但这里面有个需要注意的问题，如果我们要发送的数据比较小的话，mpi会缓存我们的数据，也就是说执行到send这个代码的时候，会缓存被send的数据，然后继续执行后面的指令，而不会等待对方进程执行recv指令接收完这个数据。但是，如果要发送的数据很大，那么进程就是挂起等待，直到接收进程执行了recv指令接收了这个数据，进程才继续往下执行。所以上述的代码发送[rank]\*5没啥问题，如果发送[rank]\*500程序就会半死不活的样子了。因为所有的进程都会卡在发送这条指令，等待下一个进程发起接收的这个指令，但是进程是执行完发送的指令才能执行接收的指令，这就和死锁差不多了。所以一般，我们将其修改成以下的方式：

```
import mpi4py.MPI as MPI
comm = MPI.COMM_WORLD
comm_rank = comm.Get_rank()
comm_size = comm.Get_size()
data_send = [comm_rank]*5
if comm_rank == 0:
   comm.send(data_send, dest=(comm_rank+1)%comm_size)
if comm_rank > 0:
   data_recv = comm.recv(source=(comm_rank-1)%comm_size)
   comm.send(data_send, dest=(comm_rank+1)%comm_size)
if comm_rank == 0:
   data_recv = comm.recv(source=(comm_rank-1)%comm_size)
print("my rank is %d, and Ireceived:" % comm_rank)
print data_recv
```

第一个进程一开始就发送数据，其他进程一开始都是在等待接收数据，这时候进程1接收了进程0的数据，然后发送进程1的数据，进程2接收了，再发送进程2的数据……知道最后进程0接收最后一个进程的数据，从而避免了上述问题。

一个比较常用的方法是封一个组长，也就是一个主进程，一般是进程0作为主进程leader。主进程将数据发送给其他的进程，其他的进程处理数据，然后返回结果给进程0。换句话说，就是进程0来控制整个数据处理流程。

## 2.3、群体通信

点对点通信是A发送给B，一个人将自己的秘密告诉另一个人，群体通信（Collective Communications）像是拿个大喇叭，一次性告诉所有的人。前者是一对一，后者是一对多。但是，群体通信是以更有效的方式工作的。它的原则就一个：尽量把所有的进程在所有的时刻都使用上！我们在下面的bcast小节讲述。


群体通信还是发送和接收两类，一个是一次性把数据发给所有人，另一个是一次性从所有人那里回收结果。

### 1）广播bcast

将一份数据发送给所有的进程。例如我有200份数据，有10个进程，那么每个进程都会得到这200份数据。

```
import mpi4py.MPI as MPI
comm = MPI.COMM_WORLD
comm_rank = comm.Get_rank()
comm_size = comm.Get_size()
if comm_rank == 0:
   data = range(comm_size)
data = comm.bcast(data if comm_rank == 0else None, root=0)
print 'rank %d, got:' % (comm_rank)
print data
```

结果如下：

```
rank 0, got:
[0, 1, 2, 3, 4]
rank 1, got:
[0, 1, 2, 3, 4]
rank 2, got:
[0, 1, 2, 3, 4]
rank 3, got:
[0, 1, 2, 3, 4]
rank 4, got:
[0, 1, 2, 3, 4]
```

Root进程自己建了一个列表，然后广播给所有的进程。这样所有的进程都拥有了这个列表。然后爱干嘛就干嘛了。


对广播最直观的观点是某个特定进程将数据一一发送给每个进程。假设有n个进程，那么假设我们的数据在0进程，那么0进程就需要将数据发送给剩下的n-1个进程，这是非常低效的，复杂度是O(n)。那有没有高效的方式？一个最常用也是非常高效的手段是规约树广播：收到广播数据的所有进程都参与到数据广播的过程中。首先只有一个进程有数据，然后它把它发送给第一个进程，此时有两个进程有数据；然后这两个进程都参与到下一次的广播中，这时就会有4个进程有数据，……，以此类推，每次都会有2的次方个进程有数据。通过这种规约树的广播方法，广播的复杂度降为O(log n)。这就是上面说的群体通信的高效原则：充分利用所有的进程来实现数据的发送和接收。

### 2）散播scatter

将一份数据平分给所有的进程。例如我有200份数据，有10个进程，那么每个进程会分别得到20份数据。

```
import mpi4py.MPI as MPI
comm = MPI.COMM_WORLD
comm_rank = comm.Get_rank()
comm_size = comm.Get_size()
if comm_rank == 0:
   data = range(comm_size)
   print data
else:
   data = None
local_data = comm.scatter(data, root=0)
print 'rank %d, got:' % comm_rank
print local_data
```

结果如下：

```
[0, 1, 2, 3, 4]
rank 0, got:
0
rank 1, got:
1
rank 2, got:
2
rank 3, got:
3
rank 4, got:
4
```

这里root进程创建了一个list，然后将它散播给所有的进程，相当于对这个list做了划分，每个进程获得等分的数据，这里就是list的每一个数。（主要根据list的索引来划分，list索引为第i份的数据就发送给第i个进程）。如果是矩阵，那么就等分的划分行，每个进程获得相同的行数进行处理。


需要注意的是，MPI的工作方式是每个进程都会执行所有的代码，所以每个进程都会执行scatter这个指令，但只有root执行它的时候，它才兼备发送者和接收者的身份（root也会得到属于自己的数据），对于其他进程来说，他们都只是接收者而已。

### 3）收集gather

那有发送，就有一起回收的函数。Gather是将所有进程的数据收集回来，合并成一个列表。下面联合scatter和gather组成一个完成的分发和收回过程：

```
import mpi4py.MPI as MPI
comm = MPI.COMM_WORLD
comm_rank = comm.Get_rank()
comm_size = comm.Get_size()
if comm_rank == 0:
   data = range(comm_size)
   print data
else:
   data = None
local_data = comm.scatter(data, root=0)
local_data = local_data * 2
print 'rank %d, got and do:' % comm_rank
print local_data
combine_data = comm.gather(local_data,root=0)
if comm_rank == 0:
printcombine_data
```

结果如下：

```
[0, 1, 2, 3, 4]
rank 0, got and do:
0
rank 1, got and do:
2
rank 2, got and do:
4
rank 4, got and do:
8
rank 3, got and do:
6
[0, 2, 4, 6, 8]
```

Root进程将数据通过scatter等分发给所有的进程，等待所有的进程都处理完后（这里只是简单的乘以2），root进程再通过gather回收他们的结果，和分发的原则一样，组成一个list。Gather还有一个变体就是allgather，可以理解为它在gather的基础上将gather的结果再bcast了一次。啥意思？意思是root进程将所有进程的结果都回收统计完后，再把整个统计结果告诉大家。这样，不仅root可以访问combine_data，所有的进程都可以访问combine_data了。

### 4）规约reduce

规约是指不但将所有的数据收集回来，收集回来的过程中还进行了简单的计算，例如求和，求最大值等等。为什么要有这个呢？我们不是可以直接用gather全部收集回来了，再对列表求个sum或者max就可以了吗？这样不是累死组长吗？为什么不充分使用每个工人呢？规约实际上是使用规约树来实现的。例如求max，完成可以让工人两两pk后，再返回两两pk的最大值，然后再对第二层的最大值两两pk，直到返回一个最终的max给组长。组长就非常聪明的将工作分配下工人高效的完成了。这是O(n)的复杂度，下降到O(log n)（底数为2）的复杂度。

```
import mpi4py.MPI as MPI
comm = MPI.COMM_WORLD
comm_rank = comm.Get_rank()
comm_size = comm.Get_size()
if comm_rank == 0:
   data = range(comm_size)
   print data
else:
   data = None
local_data = comm.scatter(data, root=0)
local_data = local_data * 2
print 'rank %d, got and do:' % comm_rank
print local_data
all_sum = comm.reduce(local_data, root=0,op=MPI.SUM)
if comm_rank == 0:
print 'sumis:%d' % all_sum
```

结果如下：

```
[0, 1, 2, 3, 4]
rank 0, got and do:
0
rank 1, got and do:
2
rank 2, got and do:
4
rank 3, got and do:
6
rank 4, got and do:
8
sum is:20
```

可以看到，最后可以得到一个sum值。

# 三、常见用法

## 3.1、对一个文件的多个行并行处理

```
#!usr/bin/env python
#-*- coding: utf-8 -*-
import sys
import os
import mpi4py.MPI as MPI
import numpy as np
#
#  Global variables for MPI
#
# instance for invoking MPI relatedfunctions
comm = MPI.COMM_WORLD
# the node rank in the whole community
comm_rank = comm.Get_rank()
# the size of the whole community, i.e.,the total number of working nodes in the MPI cluster
comm_size = comm.Get_size()
if __name__ == '__main__':
   if comm_rank == 0:
       sys.stderr.write("processor root starts reading data...\n")
       all_lines = sys.stdin.readlines()
   all_lines = comm.bcast(all_lines if comm_rank == 0 else None, root = 0)
   num_lines = len(all_lines)
   local_lines_offset = np.linspace(0, num_lines, comm_size +1).astype('int')
   local_lines = all_lines[local_lines_offset[comm_rank] :local_lines_offset[comm_rank + 1]]
   sys.stderr.write("%d/%d processor gets %d/%d data \n" %(comm_rank, comm_size, len(local_lines), num_lines))
   cnt = 0
   for line in local_lines:
       fields = line.strip().split('\t')
       cnt += 1
       if cnt % 100 == 0:
           sys.stderr.write("processor %d has processed %d/%d lines \n" %(comm_rank, cnt, len(local_lines)))
       output = line.strip() + ' process every line here'
       print output
```

## 3.2、对多个文件并行处理

如果我们的文件太大，例如几千万行，那么mpi是没办法将这么大的数据bcast给所有的进程的，所以我们可以先把大的文件split成小的文件，再让每个进程处理少数的文件。

```
import sys
import os
import mpi4py.MPI as MPI
import numpy as np
#
#  Global variables for MPI
#
# instance for invoking MPI relatedfunctions
comm = MPI.COMM_WORLD
# the node rank in the whole community
comm_rank = comm.Get_rank()
# the size of the whole community, i.e.,the total number of working nodes in the MPI cluster
comm_size = comm.Get_size()
if __name__ == '__main__':
   if len(sys.argv) != 2:
       sys.stderr.write("Usage: python *.py directoty_with_files\n")
       sys.exit(1)
   path = sys.argv[1]
   if comm_rank == 0:
       file_list = os.listdir(path)
       sys.stderr.write("%d files\n" % len(file_list))
   file_list = comm.bcast(file_list if comm_rank == 0 else None, root = 0)
   num_files = len(file_list)
   local_files_offset = np.linspace(0, num_files, comm_size +1).astype('int')
   local_files = file_list[local_files_offset[comm_rank] :local_files_offset[comm_rank + 1]]
   sys.stderr.write("%d/%d processor gets %d/%d data \n" %(comm_rank, comm_size, len(local_files), num_files))
    cnt = 0
   for file_name in local_files:
       hd = open(os.path.join(path, file_name))
       for line in hd:
           output = line.strip() + ' process every line here'
           print output
       cnt += 1
       sys.stderr.write("processor %d has processed %d/%d files \n" %(comm_rank, cnt, len(local_files)))
       hd.close()
```

## 3.3、联合numpy对矩阵的多个行或者多列并行处理

Mpi4py一个非常优秀的特性是完美支持numpy！

```
import os, sys, time
import numpy as np
import mpi4py.MPI as MPI
#
#  Global variables for MPI
#
# instance for invoking MPI relatedfunctions
comm = MPI.COMM_WORLD
# the node rank in the whole community
comm_rank = comm.Get_rank()
# the size of the whole community, i.e.,the total number of working nodes in the MPI cluster
comm_size = comm.Get_size()
# test MPI
if __name__ == "__main__":
    #create a matrix
   if comm_rank == 0:
       all_data = np.arange(20).reshape(4, 5)
       print "************ data ******************"
       print all_data
  
    #broadcast the data to all processors
   all_data = comm.bcast(all_data if comm_rank == 0 else None, root = 0)
  
    #divide the data to each processor
   num_samples = all_data.shape[0]
   local_data_offset = np.linspace(0, num_samples, comm_size + 1).astype('int')
  
    #get the local data which will be processed in this processor
   local_data = all_data[local_data_offset[comm_rank] :local_data_offset[comm_rank + 1]]
   print "****** %d/%d processor gets local data ****" %(comm_rank, comm_size)
   print local_data
  
    #reduce to get sum of elements
   local_sum = local_data.sum()
   all_sum = comm.reduce(local_sum, root = 0, op = MPI.SUM)
  
    #process in local
   local_result = local_data ** 2
  
    #gather the result from all processors and broadcast it
   result = comm.allgather(local_result)
   result = np.vstack(result)
  
   if comm_rank == 0:
       print "*** sum: ", all_sum
       print "************ result ******************"
       print result
```

# 四、MPI和mpi4py的环境搭建

这章放到这里是作为一个附录。我们的环境是linux，需要安装的包有python、openmpi、numpy、cpython和mpi4py，过程如下：

## 4.1、安装Python

```
#tar xzvf Python-2.7.tgz
#cd Python-2.7
#./configure--prefix=/home/work/vis/zouxiaoyi/my_tools
#make
#make install
```

先将Python放到环境变量里面，还有Python的插件库

```
exportPATH=/home/work/vis/zouxiaoyi/my_tools/bin:$PATH
exportPYTHONPATH=/home/work/vis/zouxiaoyi/my_tools/lib/python2.7/site-packages:$PYTHONPATH
```

执行#python，如果看到可爱的>>>出来，就表示成功了。按crtl+d退出

## 4.2、安装openmpi

```
#wget http://www.open-mpi.org/software/ompi/v1.4/downloads/openmpi-1.4.1.tar.gz
#tar xzvf openmpi-1.4.1.tar.gz
#cd openmpi-1.4.1
#./configure--prefix=/home/work/vis/zouxiaoyi/my_tools
#make -j 8
#make install
```

然后把bin路径加到环境变量里面：

```
exportPATH=/home/work/vis/zouxiaoyi/my_tools/bin:$PATH
exportLD_LIBRARY_PATH=/home/work/vis/zouxiaoyi/my_tools/lib:$LD_LIBRARY_PATH
```

执行#mpirun，如果有帮助信息打印出来，就表示安装好了。需要注意的是，我安装了几个版本都没有成功，最后安装了1.4.1这个版本才能成功，因此就看你的人品了。

## 4.3、安装numpy和Cython

安装python库的方法可以参考之前的博客。过程一般如下：

```
#tar –xgvf Cython-0.20.2.tar.gz
#cd Cython-0.20.2
#python setup.py install
```

打开Python，import Cython，如果没有报错，就表示安装成功了

## 4.4、安装mpi4py

```
#tar –xgvf mpi4py_1.3.1.tar.gz
#cd mpi4py
#vi mpi.cfg
```

在68行，[openmpi]下面，将刚才已经安装好的openmpi的目录给改上。

```
mpi_dir = /home/work/vis/zouxiaoyi/my_tools
#python setup.py install
```

打开Python，import mpi4py as MPI，如果没有报错，就表示安装成功了


下面就可以开始属于你的并行之旅了，勇敢探索多核的乐趣吧。
