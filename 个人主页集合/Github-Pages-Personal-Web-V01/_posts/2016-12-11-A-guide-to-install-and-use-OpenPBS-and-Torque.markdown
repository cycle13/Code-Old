---
layout:     post
title:      "OpenPBS/Torque安装与使用指南"
subtitle:   "A guide to install and use OpenPBS and Torque"
date:       2016-12-11
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Linux系统运维与服务器管理
---

PBS是由NAS（National Academy of Sciences）开发的面向批作业调度以及系统资源管理的软件包。它主要用于管理使用UNIX或Linux的同构或异构的机群系统。PBS是一个商用软件，拥有完善的解决方案和技术支持，但是价格昂贵。OpenPBS是对PBS系统的开源实现，遵循开源软件的相关约定，所以任何个人和组织都可以自由获得 其源代码并修改之。由于笔者进行安装调试的时间较早，本文介绍的对象为OpenPBS 2.3.16版，可能与现在的最新版有些许不同，但大体应该一样。
请注意，在使用OpenPBS 2.3.16提交作业时，提交者用户名长度不能大于15个字符。

## OpenPBS的结构

OpenPBS主要由三个主要部件组成：
PBS服务守护进程： pbs_server 负责接收作业提交，位于服务节点
PBS调度守护进程： pbs_sched 负责调度作业，位于服务节点
PBS MOM守护进程： pbs_mom 负责监控本机并执行作业，位于所有计算节点

## 在单个节点上安装OpenPBS

笔者曾在自己安装Fedora操作系统的台式机上安装调试OpenPBS与Torque。这是当时的一些学习笔记。

由于整个OpenPBS都被安装在一台计算机上，所以上述pbs_server、pbs_mom和pbs_sched都会被安装在一起。
具体步骤如下：

- #### 1. 安装前提：
a)机器上安装有合适版本的Linux、gcc编译器以及其他所需的包<br/>
b)安装者能以root帐户登录计算机；<br/>
c)下载合适的源码形式安装包，如pbs.tar.gz<br/>

- #### 2. 以root帐号登录计算机

- #### 3. 用tar zxf pbs.tar.gz命令释放安装包到指定位置如/opt/OpenPBS_2_3_16/

- #### 4. 进入解压得到的目录，执行命令：
a)`./configure –disable-gui–set-server-home={YOUR_PBS_HOME}`说明：有的环境下可能是`—with-server-home`，具体可以通过`./configure –-help`查看<br/>
b)`./configure –enable-docs –disable-gui`其中：
`–disable-gui`：说明不安装GUI组件，因为该组件不常用，且对linux的tcl等库的版本颇为挑剔，往往使安装进程无法继续
`–set-server-home`： 指定OpenPBS的工作目录，默认是 `/usr/spool/PBS/`

- #### 5. 执行`make`

- #### 6. 执行`make install`，这样就可以把OpenPBS安装到指定工作目录中

- #### 7. 下面设置OpenPBS（假设工作目录为`/usr/spool/PBS/`）
a)设置服务器名：编辑`/usr/spool/PBS/server_name`文件，填写本机机器名（如cngrid217）<br/>
b)设置机群所有机器名：编辑`/usr/spool/PBS/server_priv/nodes`文件，填写本机机器名<br/>
c)设置MOM进程配置文件：编辑 `/usr/spool/PBS/mom_priv/config`文件，写入如下内容
`\$logevent 0×1ff`
`\$clienthost server_host`
其中`server_host`是本机的机器名，譬如mydemocluster

- #### 8. 启动OpenPBS
a)启动mom进程：(任意路径下)执行命令 `pbs_mom`<br/>
b)启动调度器进程:执行命令 `pbs_sched`<br/>
c)启动OpenPBS服务器进程，创建pbs数据库
执行命令 `pbs_server -t create`
其参数 `-t create` 只在首次启动server进程时才需要

- #### 9. 创建并设置作业队列：
a) 用qmgr创建队列normal：`qmgr -c "c q normal"`其中：
`-c` 表示其后是命令
`c` 表示创建
`q` 表示队列<br/>
b) 设定队列的类型为可执行队列`qmgr -c "s q normal queue_type=Execution"`其中`s`表示设置。<br/>
c) 设定队列中任务的最大运行时间为24小时(CPU时间)`qmgr -c "s q normal resources_max.cput=24:00:00"`<br/>
d) 设定该队列中任务最小运行时间为1秒(CPU时间)`qmgr -c "s q normal resources_min.cput=1"`<br/>
e) 设定该队列中任务默认运行时间为12分钟(CPU时间)`qmgr -c "s q normal resources_default.cput=12:00"`<br/>
f) 使队列生效`qmgr -c "s q normal enabled=true"`<br/>
g) 启用队列`qmgr -c "s q normal started=true"`<br/>
h) 将normal队列设定为默认队列`qmgr -c "s s default_queue=normal"`<br/>
i) 在创建完成normal队列后，还可按照a～h各步骤创建拥有其他属性的队列<br/>
j) 为了方便安装过程，同时也避免重复操作时出错，可使用已有的配置文件完成队列设置工作：<br/>
i. 设配置文件名为 `pbsconf`<br/>
ii. 使用命令 `qmgr < pbsconf` 导入队列设置

- #### 10. 将PBS做成系统服务，以便随时启动、关闭它
a)编辑`/etc/pbs.conf`文件，填入如下内容：
    `pbs_home=/usr/spool/PBS`
    `pbs_exec=/usr/local`
    `start_server=1`
    `start_sched=1`
    `start_mom=1`<br/>
b) 将OpenPBS解压目录中的 `……/src/tools/init.d/pbs`文件复制到`/etc/init.d/pbs`<br/>
c) 执行命令`chkconfig –add pbs`<br/>
d) 用`chkconfig –list | grep pbs`<br/> 命令查看pbs服务是否已被加入
e) 现在就可以使用如下命令启动、关闭pbs了：<br/>
i. 启动pbs： `service pbs start`<br/>
ii. 关闭pbs： `service pbs stop`<br/>
iii. 重启pbs： `service pbs restart`

- #### 11. 检查安装是否成功：
a) 进入一个非root帐户<br/>
b) 执行命令 `echo hostname >test.pbs` 创建一个返回机器名的pbs脚本ben<br/>
c) 执行`qsub test.pbs` 命令，提交作业<br/>
d) 如果在test.pbs所在目录下出现test.pbs.o×（其中×是以0开始的某个非负整数），且其内容为本机机器名，则说明openPBS已经开始正常工作

## 在多节点机群上安装OpenPBS

由于缺乏硬件条件，笔者也没有亲自在多节点集群上安装过OpenPBS和Torque，下面的仅仅是学习笔记。

在这多个节点之间应有一个节点作为服务节点，负责调度作业，其他节点是计算节点，负责接收作业并计算之。（服务节点自身也可承担计算任务）
由于OpenPBS由三个部件组成，则在机群内应根据角色不同在节点上安装不同的OpenPBS组件。详细而言，在服务节点上应安装PBS服务守护进程 （pbs_server)、MOM进程(pbs_mom)和作业调度器（pbs_sched)，而在从节点上应该只安装MOM进程。
不过为了安装和以后变换节点角色方便，这里可以在所有节点上都安装这三个组件，只不过在启动OpenPBS时，服务节点三个组件全部启动，而计算节点只启动MOM进程。
这里假设有N台节点机，机器名分别为node1、node2、……、nodeN；确定node1为服务节点，所有节点都是计算节点。
具体安装步骤：

- #### 1. 安装前提：
a) 各节点上都有合适版本的Linux和gcc编译器<br/>
b) 各节点之间能互连<br/>
c) 安装者能在每个节点上以root身份登录<br/>
d) 已下载合适的pbs源码安装包<br/>

- #### 2 在所有节点上执行：
a) 用root身份登录<br/>
b) 用`tar -zxf pbs.tar.gz`命令释放安装包到指定位置，譬如：/opt/OpenPBS_2_3_16/<br/>
c) 进入解压所得目录，通过configure脚本完成初始配置
    `./configure –disable-gui –set-default-server {serverName}`
其中：
`–set-default-server` 指定该机群系统的作业服务守护进程所在主机名，在本例中就是node1<br/>
d) 执行 `make`<br/>
e) 执行 `make install`<br/>
f) 设置MOM进程配置文件：
编辑 `/usr/spool/PBS/mom_priv/config`文件，写入如下内容
`\$logevent　0×1ff`
`\$clienthost server_host`
其中server_host是机群服务节点的机器名，譬如在本例中是node1<br/>
g) 设置server_name：
编辑 `/usr/spool/PBS/server_name`文件，写入机群服务节点的机器名，譬如在本例中是node1

- #### 3. 在各个计算节点上分别启动MOM守护进程：
执行 `pbs_mom`

- #### 4. 在机群服务节点上执行如下步骤：
a) 设置PBS服务端配置
编辑 `/usr/spool/PBS/server_pri/nodes` 文件，写入所有节点的机器名
譬如：
node1
node2
……
nodeN<br/>
b) 启动pbs服务器进程
执行命令 `pbs_server -t create`
其参数 `-t create` 只在首次启动server进程时才需要<br/>
c) 启动pbs调度器进程
执行命令 `pbs_sched`<br/>
d) 配置队列（参见前一部分配置队列操作）
如果已经有一个队列配置文件，譬如`pdfconf`，则可使用命令：
`qmgr < pbsconf` 命令直接配置所有队列

- #### 5. 将PBS做成系统服务，以便随时启动、关闭它
a) 在机群服务节点上编辑`/etc/pbs.conf`文件，填入如下内容：
`pbs_home=/usr/spool/PBS`
`pbs_exec=/usr/local`
`start_server=1`
`start_sched=1`
`start_mom=1`
b) 在机群计算节点（除服务节点外）上编辑`/etc/pbs.conf`文件，填入如下内容：
`pbs_home=/usr/spool/PBS`
`pbs_exec=/usr/local`
`start_server=0`
`start_sched=0`
`start_mom=1`
c) 将各个节点OpenPBS解压目录中的
    `……/src/tools/init.d/pbs`
复制到
    `/etc/init.d/pbs`
<br/>
d) 在各节点执行命令`chkconfig –add pbs`<br/>
e) 在各节点用`chkconfig –list | grep pbs` 命令查看pbs服务是否已被加入<br/>
f) 现在就可以在各个节点使用如下命令启动、关闭pbs了：<br/>
i. 启动pbs： `service pbs start`<br/>
ii. 关闭pbs： `service pbs stop`<br/>
iii. 重启pbs： `service pbs restart`

- #### 6. 检查安装是否成功
a) 在任意节点上进入非root身份<br/>
e) 进入nfs某个目录，执行命令 `echo hostname >test.pbs` 创建一个返回机器名的pbs脚本<br/>
f) 执行`qsub test.pbs` 命令，提交作业<br/>
g) 如果在test.pbs所在目录下出现test.pbs.o×（其中×是以0开始的某个非负整数），且其内容为前述`/usr/spool/PBS/server_pri/nodes`文件中位于第一行的主机名，则说明openPBS已经开始正常工作

## 在AMD64optern服务器上安装OpenPBS
这部分内容仅仅是笔记。实际上PBS作业调度系统在笔者安装调试时已经可以安装在64位系统上了。

前面两节所介绍的安装方法针对的是32位版本RedHatLinux AS3/4。事实上随着64位服务器的普及，目前常需要在64位的linux操作系统上安装OpenPBS。不过由于OpenPBS 2.3.16已有多年历史，尚不能直接安装在64位操作系统上，因此需要在安装前对OpenPBS安装包作一定的调整和修正。具体方法如下：

- #### 1. 用`tar zxf pbs.tar.gz`命令释放安装包到指定位置，譬如：`/opt/OpenPBS_2_3_16/`

- #### 2. 修改 `/opt/OpenPBS_2_3_16/buildutils/config.sub`
a) 在第212行把：
`Recognize the basic CPU types with company name.`
`vax-* | tahoe-* | i[3456]86-* | i860-* | ….`
改为：
`vax-* | tahoe-* | i[3456]86-* | x86_64-* | i860-* | ….`
<br/>
b) 第229行在：
`Recognize the various machine names and aliases which stand`
`for a CPU type and a company and sometimes even an OS`
在后面添加：
`i*86 | x86_64)`
`basic_machine=$basic_machine-pc`
`;;`

- #### 3. 修改 `/opt/OpenPBS_2.3.16/buildutils/makedepend-sh`
第576行把：
`\$CPP \$arg_cc \$d/\$s \$errout |`
`sed -n -e "s;^\# [0-9][0-9 ]*"(.*)";$f: 1;p" |`
`grep -v "$s$" |`
`sed -e "s;([^ :]*: [^ ]*).*;1;"`
`>> $TMP`
改为：
`\$CPP \$arg_cc \$d/$s \$errout |`
`sed -n -e "s;^# [0-9][0-9 ]*"(.*)";$f: 1;p" |`
`grep -v "$s$" |`
`grep -v ">$" |`
`sed -e "s;([^ :]*: [^ ]*).*;1;"`
`>> $TMP`

- #### 4. 修改`…/OpenPBS_2.3.16/src/lib/Liblog/pbs_log.c`
第92行`include <pbs_config.h>`
在下面增加一行`include <errno.h>`

- #### 5. 修改`…/OpenPBS_2.3.16/src/server/svr_connect.c`
第102行`include <pbs_config.h>`
在下面增加一行`include <errno.h>`

- #### 6. 按照前面两部分所述内容，完成make、make install、配置等过程。

## Torque 
笔者曾在单机上安装过Torque，Torque比OpenPBS安装简单。下面是当时的笔记。

Torque是作业队列管理系统，其前身是openPBS，但后来openPBS的那帮人开公司做商业PBS赚钱去了，openPBS改为Torque继续为开源社区维护。

关于Torque的安装，网上的教程很多。但是，这些教程大多是针对cluster集群的，有的是torque不同版本不同平台的，所以给出的设置各不相同。一旦你按照一个不合适的教程去操作，就会导致诸如找不到服务器无法安装，或者安装后作业提交了不能运行，或者能提交作业但立即结束退出，或者作业能正常运行但不给出o文件等各种问题。针对各种问题的解决方案更是千奇百怪，让人云里雾里，无所适从。

不幸的是，上面提到的问题我全部遇到了。各种国内外网站给出的解决方案几乎都折腾遍，才逐渐意识到，安装Torque，必须搞清Torque的三个部分，pbs_server, pbs_mom, pbs_sched， 它们之间的关系和通信机制的关键。这一部分推荐去百度或道客上搜几篇关于“作业调度系统PBS”介绍的ppt看看。简单说，
pbs_server是领导，坐在总部服务器上负责接收任务，
pbs_sched是经理，负责把工作排序并分配下去，
pbs_mom是民工，在各个计算节点辛苦工作，并把情况汇报给总部。

三个部分的配置有2个关键，

关键设置A，你必须告诉server和sched每个节点的名字和核的数量，以便它们分配任务<br/>
关键设置B，是你必须告诉mom哪个节点是服务器节点，以便其向总部汇报工作进展。

具体来所，两个关键设置涉及如下操作：（配置均在torque默认安装目录下：/var/spool/torque）<br/>
关键设置A： 创建或修改server_priv/nodes文件，列出计算节点名称和核心数；<br/>
关键设置B： 创建或修改mom_priv/config文件列出主节点ip；创建server_name文件，列出主节点hostname

![img](/img/in-post/2016-12-11-A-guide-to-install-and-use-OpenPBS-and-Torque/01.jpg)

对于工作站来说，和cluster集群的唯一区别是，工作站本身既是服务节点，又是计算节点。作为服务器，本机IP和主机名默认分别为127.0.0.1和torqueserver。 另外还要再给它设置一个别名，比如calnode1，作为计算节点的名称。如上面图所示。这里搞不清，很容易出错。

好了，明白了以上关键部分，下面就简单了。在我的linuxmint下，配置一台既是服务节点又是计算节点的4核工作站的流程：<br/>

- #### 1. 修改/etc/hosts第一行，使其为“127.0.0.1 localhost yourhostname torqueserver calnode1” (root权限)<br/>
\$ echo \$HOSTNAME   //  find the hostname<br/>
xxxxx  //  write this hostname into /etc/hosts<br/>
\$ sudo vi /etc/hosts <br/>
127.0.0.1 localhost xxxxx torqueserver calnode1<br/>
\# 127.0.1.1  somename   // 这一行要comment掉<br/>
后面有一些ipv6的东西，无需动。

- #### 2. 安装torque的7个相关包
\$ sudo apt-get install torque-common libtorque2 libtorque2-dev torque-server torque-scheduler torque-mom torque-client <br/>
安装完成后server, sched会自动启动

- #### 3. 初始化
\$ sudo qterm  // 先终止服务 <br/>
\$ sudo bash /usr/share/doc/torque-common/torque.setup $USER torqueserver  // 建立默认服务器和队列，并把自己列为管理员<br/>
\$ qmgr -c 'print server'  // 查看默认配置的服务和队列

\#

\# Create queues and set their attributes.

\#

\#

\# Create and define queue batch

\#

create queue batch

set queue batch queue_type = Execution

set queue batch resources_default.nodes = 1

set queue batch resources_default.walltime = 01:00:00

set queue batch enabled = True

set queue batch started = True

\#

\# Set server attributes.

\#

set server scheduling = True

set server acl_hosts = torqueserver

set server default_queue = batch

set server log_events = 511

set server mail_from = adm

set server scheduler_iteration = 600

set server node_check_rate = 150

set server tcp_timeout = 6

set server mom_job_sync = True

set server keep_completed = 300  // 作业完成后会等待300秒才消失，这里需要改成1,见本文附录。

- #### 4. 设置服务节点<br/>
(1) 创建server_name文件，指明服务节点的名称为torqueserver<br/>
\$ sudo echo "torqueserver" > /var/spool/torque/server_name  // 此文件应该是默认已经自动生成的<br/>
(2) 添加计算节点<br/> 
创建server_priv/nodes文件，指定利用名为calnode的节点的4个核做计算<br/>
\$ sudo echo "calnode1 np=4" ><br/> /var/spool/torque/server_priv/nodes

- #### 5. 去计算节点配置。<br/> 由于我们是工作站，所以实际上就只是在本机上操作<br/>
创建mom_priv/config文件，告诉mom向IP为127.0.0.1的服务节点汇报<br/>
\$ sudo echo "$pbs_server = 127.0.0.1” > /var/spool/torque/mom_priv/config

- #### 6. 结束配置，重启服务<br/>
先启动计算节点服务：<br/>
\$ sudo pbs_mom   <br/>
然后是服务节点<br/>
\$ sudo qterm -t quick  // 或者 $ sudo killall -r "pbs_*"<br/>
\$ sudo pbs_server  // 启动server<br/>
\$ pbsnodes -a   // 查看所有计算节点，free为正常

- #### 7. 配置服务的开机启动<br/>
\$ sudo vi /etc/rc.local 增加三列分别是pbs_server pbs_sched pbs_mom

- #### 8. 测试<br/>
\$ echo 'sleep 20' | qsub

- #### 9. 出错后根据作业号追查作业详情<br/>
\$ tracejob xx

典型作业提交脚本：

\#!/bin/bash

\#PBS -N test                 // job list显示的作业名称。通常无需指定，将显示脚本文件名

\#PBS -l ncpus=2           // 用2个核

\#PBS -l walltime=24:00:00  // 运行时间，通常在自己的工作站上无需指定

\#PBS -j oe                    // 合并o文件和e文件为o文件，这个很有用

\#PBS -q batch             // 交到batch队列，一般无需指定

\#PBS -V                    // 使用.bashrc中设置的环境变量，非常重要

cd $PBS_O_WORKDIR     // 进入脚本提交的目录为工作目录，这一行很重要。

g09  input.gjf output.log      // 作业行


附： 修改服务和队列的常用命令

(1) 创建与修改作业队列batch

\$ sudo qmgr -c 'create queue batch'  // 创建名为batch的队列

\$ sudo qmgr -c 'set queue batch queue_type = Execution'  // 类型为计算

\$ sudo qmgr -c 'set queue batch enabled = True'  // 激活

\$ sudo qmgr -c 'set queue batch started = True'  // 开启

\$ sudo qmgr -c 'set queue batch resources_default.walltime = 900:00:00'  // 最长运行时间900小时

\$ sudo qmgr -c 'set queue batch resources_default.ncpus = 1'  // 默认只用1核

\$ sudo qmgr -c 'set queue batch resources_default.nodes = 1'  // 默认使用1个节点

\$ sudo qmgr -c 'set queue batch resources_default.nodect = 1'  // 只放开1个节点

\$ sudo qmgr -c 'set queue batch resources_max.ncpus = 4'  // 最多使用4核

\$ sudo qmgr -c 'set queue batch resources_min.ncpus = 1'

\$ sudo qmgr -c 'set queue batch resources_max.nodes = 1'  // 只有1个节点

\$ sudo qmgr -c 'set queue batch max_running = 2'  // 最多同时运行2个作业


(2) 配置与修改服务器server

\$ sudo qmgr -c 'set server scheduling = True'  //  启动排队管理

\$ sudo qmgr -c 'set server default_queue = batch'  //  定义默认队列

\$ sudo qmgr -c 'set server allow_node_submit = True'  // 允许向服务节点提交作业，这个必须设置

\$ sudo qmgr -c 'set server query_other_jobs = True'  //  

\$ sudo qmgr -c 'set server acl_host_enable = True'

\$ sudo qmgr -c 'set server acl_hosts = calnode1'


## PS:

- #### 1. 关于Unauthorized request问题：

可能是忘了使用管理员权限操作；也可能是操作顺序不对，配置冲突。可杀掉所有pbs_*服务，再开启。不行重启主机。

- #### 2. o文件中的“Command not found”

很多软件的运行环境是在.bashrc中设置，但是torque在qsub时默认并不执行.bashrc，就会导致作业提交后立即结束，o文件显示command not found。解决此问题只需在作业脚本中增加一行 \#PBS -V

- #### 3. 关于PBS在机器重启后不能正常运行

0.进入root<br/>
1.trqauthd<br/>
2.pbs_server<br/>
3.pbs_mom<br/>
4.pbsnodes -a    看到free<br/>
5.maui

- #### 4. 任务无法删除

出现下面这种报错：<br/>
qdel: Request invalid for state of job MSG=invalid state for job - EXITING 6855.hn<br/>
qdel -p 任务ID可以解决，但需要root权限


- #### 5.调整任务优先级，查看任务所用节点

root权限下setspri <priority> <job>设置优先级<br/>
root权限下checkjob查看任务所用节点

- #### 6.作业调度系统脚本范例

OpenPBS：<br/>
`#!/bin/sh`<br/>
`#PBS -l nodes=4:ppn=16`<br/>
`echo "This jobs is "$PBS_JOBID@$PBS_QUEUE`<br/>
``NSLOTS=`cat ${PBS_NODEFILE} | wc -l` ``<br/>
`cd $PBS_O_WORKDIR`<br/>
`/home/qianqf/Program/openmpi/bin/mpirun -machinefile $PBS_NODEFILE -np $NSLOTS ./cam < namelist`

清华探索二号：<br/>
`#!/bin/sh`<br/>
`#BSUB -q short`<br/>
`#BSUB -a openmpi`<br/>
`#BSUB -n 48`<br/>
`#BSUB -o OUTPUTFILE.%J`<br/>
`#BSUB -e ERRORFILE.%J`<br/>
`mpirun.lsf ./wrf.exe`
