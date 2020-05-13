---
layout:     post
title:      "CESM使用指南"
subtitle:   "A simple guide for CESM"
date:       2016-12-12
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - 天气预报与数值模式
    - 我的笔记
---

> [CESM](http://www.cesm.ucar.edu/index.html)(Community Earth System Model)是一个由美国国家大气研究中心(NCAR)开发的气候系统模式，目前得到广泛的应用。本文是使用笔者一开始学习CESM时的一份记录，很多操作时间一长会忘记，所以这里就备份这份笔记。


# CESM移植安装与运行
- ## 预准备
首先查看目前CESM已经有的版本：<br/>
`svn list https://svn-ccsm-release.cgd.ucar.edu/model_versions`<br/>
然后下载需要的版本：<br/>
`svn co https://svn-ccsm-release.cgd.ucar.edu/model_versions/cesm1_2_1 cesm1_2_1`<br/>
其他软件，如编译器，并行库，netCDF等需要提前安装好，笔者用的是intel编译器套件Parallel Studio，同时并行库一般用的OpenMPI或者IMPI，此外需要提前下载好CESM的输入数据。<br/>
然后利用命令：<br/>
`cd ccsm4/scripts`<br/>
`./create_newcase -l`<br/>
列出模式支持的系统信息。

- ## 设置环境变量(CEMS与CAM5)
`#For CESM`<br/>
`export CAM_ROOT=/home/qianqf/cesm1_2_1`<br/>
`export camcfg=/home/qianqf/cesm1_2_1/models/atm/cam/bld`<br/>
`export INC_NETCDF=/home/qianqf/lib4cam/include`<br/>
`export LIB_NETCDF=/home/qianqf/lib4cam/lib`<br/>
`export INC_MPI=/share/apps/openmpi-1.4.5/include`<br/>
`export LIB_MPI=/share/apps/openmpi-1.4.5/lib`<br/>
`export CSMDATA=/home2_hn/CESM/cesm-input`<br/>
`export MOD_NETCDF=/home/qianqf/lib4cam/include`

- ## 编辑config_machines.xml文件
`<machine MACH="qqf">`<br/>
`　　　<DESC>LINUX cluster</DESC>`<br/>
`　　　<OS>LINUX</OS>`<br/>
`　　　<COMPILERS>intel</COMPILERS>`<br/>
`　　　<MPILIBS>openmpi</MPILIBS>`<br/>
`　　　<RUNDIR>/home2_hn/qianqf/$CASE/run</RUNDIR>`<br/>
`　　　<EXEROOT>/home2_hn/qianqf/$CASE/bld</EXEROOT>`<br/>
`　　　<DIN_LOC_ROOT>/home2_hn/CESM/cesm-input</DIN_LOC_ROOT>`<br/>
`　　　<DIN_LOC_ROOT_CLMFORC>/home2_hn/CESM/cesm-input/atm/datm7</DIN_LOC_ROOT_CLMFORC>`<br/>
`　　　<DOUT_S>FALSE</DOUT_S>`<br/>
`　　　<DOUT_S_ROOT>/home2_hn/qianqf/$CASE</DOUT_S_ROOT>`<br/>
`　　　<DOUT_L_MSROOT>/home2_hn/qianqf/$CASE</DOUT_L_MSROOT>`<br/>
`　　　<CCSM_BASELINE>UNSET</CCSM_BASELINE>`<br/>
`　　　<CCSM_CPRNC>UNSET</CCSM_CPRNC>`<br/>
`　　　<BATCHQUERY>qstat</BATCHQUERY>`<br/>
`　　　<BATCHSUBMIT>qsub</BATCHSUBMIT>`<br/>
`　　　<SUPPORTED_BY>qqf1403321992 -at- gmail.com</SUPPORTED_BY>`<br/>
`　　　<GMAKE_J>1</GMAKE_J>`<br/>
`　　　<MAX_TASKS_PER_NODE>16</MAX_TASKS_PER_NODE>`<br/>
`</machine>`<br/>

- ## 配置env_mach_specific文件
> 由于config_machines.xml里设置了MACH="qqf"，所以env_mach_specific后缀为qqf，即env_mach_specific.qqf。但这个文件实际并不会起什么太大的作用。

`#! /bin/csh -f`<br/>
`# -------------------------------------------------------------------------`<br/>
`# USERDEFINED`<br/>
`# Edit this file to add module load or other paths needed for the build`<br/>
`# and run on the system.  Can also include general env settings for machine.`<br/>
`# Some samples are below`<br/>
`# -------------------------------------------------------------------------`<br/>
`#source /opt/modules/default/init/csh`<br/>
`#if ( $COMPILER == "pgi" ) then`<br/>
`#  module load pgi`<br/>
`#endif`<br/>
`#module load netcdf`<br/>
`#limit coredumpsize unlimited`<br/>

- ## 配置mkbatch文件
> 和env_mach_specific文件一样，根据config_machines.xml里设置了MACH="qqf"，所以后缀也是qqf，即mkbatch.qqf

`#! /bin/csh -f`<br/>
`set mach = qqf`<br/>
`#################################################################################`<br/>
`if ($PHASE == set_batch) then`<br/>
`#################################################################################`<br/>
`source ./Tools/ccsm_getenv || exit -1`<br/>
``set ntasks  = `${UTILROOT}/Tools/taskmaker.pl -sumonly` ``<br/>
``set maxthrds = `${UTILROOT}/Tools/taskmaker.pl -maxthrds` ``<br/>
`@ nodes = $ntasks / ${MAX_TASKS_PER_NODE}`<br/>
`if ( $ntasks % ${MAX_TASKS_PER_NODE} > 0) then`<br/>
`　　@ nodes = $nodes + 1`<br/>
`　　@ ntasks = $nodes * ${MAX_TASKS_PER_NODE}`<br/>
`endif`<br/>
`@ taskpernode = ${MAX_TASKS_PER_NODE} / ${maxthrds}`<br/>
`set qname = batch`<br/>
`set tlimit = "00:59:00"`<br/>
`#--- Job name is first fifteen characters of case name ---`<br/>
``set jobname = `echo ${CASE} | cut -c1-15` ``<br/>
`cat >! $CASEROOT/${CASE}.${mach}.run << EOF1`<br/>
`#PBS -l nodes=${nodes}:ppn=${taskpernode}`<br/>
`#!/bin/csh -f`<br/>
`#===============================================================================`<br/>
`# GENERIC_USER`<br/>
`# This is where the batch submission is set.  The above code computes`<br/>
`# the total number of tasks, nodes, and other things that can be useful`<br/>
`# here.  Use PBS, BSUB, or whatever the local environment supports.`<br/>
`#===============================================================================`<br/>
`##PBS -N ${jobname}`<br/>
`##PBS -q ${qname}`<br/>
`##PBS -l nodes=${nodes}:ppn=${taskpernode}`<br/>
`##PBS -l walltime=${tlimit}`<br/>
`##PBS -r n`<br/>
`##PBS -j oe`<br/>
`##PBS -S /bin/csh -V`<br/>
`##BSUB -l nodes=${nodes}:ppn=${taskpernode}:walltime=${tlimit}`<br/>
`##BSUB -q ${qname}`<br/>
`###BSUB -k eo`<br/>
`###BSUB -J $CASE`<br/>
`###BSUB -W ${tlimit}`<br/>
`#limit coredumpsize 1000000`<br/>
`#limit stacksize unlimited`<br/>
`#BSUB -J ${CASE}`<br/>
`#BSUB -q hpc_linux`<br/>
`#BSUB -n ${maxtasks}`<br/>
`#BSUB -o output.%J`<br/>
`#BSUB -e error.%J`<br/>
`EOF1`<br/>
`#################################################################################`<br/>
`else if ($PHASE == set_exe) then`<br/>
`#################################################################################`<br/>
``set maxthrds = `${UTILROOT}/Tools/taskmaker.pl -maxthrds` ``<br/>
``set maxtasks = `${UTILROOT}/Tools/taskmaker.pl -sumtasks``<br/>
`cat >> ${CASEROOT}/${CASE}.${MACH}.run << EOF1 `<br/>
`# -------------------------------------------------------------------------`<br/>
`# Run the model`<br/>
`# -------------------------------------------------------------------------`<br/>
`sleep 25`<br/>
`cd \$RUNDIR`<br/>
``echo "\`date\` -- CSM EXECUTION BEGINS HERE" ``<br/>
`#===============================================================================`<br/>
`# GENERIC_USER`<br/>
`# Launch the job here.  Some samples are commented out below`<br/>
`#===============================================================================`<br/>
`setenv OMP_NUM_THREADS ${maxthrds}`<br/>
`if (\$USE_MPISERIAL == "FALSE") then`<br/>
`　　　#`<br/>
`　　　# Find the correct mpirun command and comment it out`<br/>
`　　　# Usually it will just be mpiexec or mpirun...`<br/>
`　　　# Remove the echo and exit below when you've done so.`<br/>
`　　　#`<br/>
`　　　#echo "GENERIC_USER: Put the correct mpirun command in your *.run script, then remove this echo/exit"`<br/>
`　　　#exit 2`<br/>
`　　　mpiexec -n ${maxtasks} ./ccsm.exe >&! ccsm.log.\$LID`<br/>
`　　　#mpirun -np ${maxtasks} ./ccsm.exe >&! ccsm.log.\$LID`<br/>
`　　　#./ccsm.exe`<br/>
`else`<br/>
`　　　./ccsm.exe >&! ccsm.log.\$LID`<br/>
`endif`<br/>
`wait`<br/>
``echo "\`date\` -- CSM EXECUTION HAS FINISHED" ``<br/>
`EOF1`<br/>
`#################################################################################`<br/>
`else if ($PHASE == set_larch) then`<br/>
`#################################################################################`<br/>
`#This is a place holder for a long-term archiving script`<br/>
`#################################################################################`<br/>
`else`<br/>
`#################################################################################`<br/>
`　　　echo "mkscripts.$mach"`<br/>
`　　　echo "  PHASE setting of $PHASE is not an accepted value"`<br/>
`　　　echo "  accepted values are set_batch, set_exe and set_larch"`<br/>
`　　　exit 1`<br/>
`#################################################################################`<br/>
`endif`<br/>
`#################################################################################`<br/>

- ## 新建CASE
执行下面这条命令：<br/>
`./create_newcase -case test -res f19_g16 -compset X -mach qqf`<br/>
`-case`：新建的CASE的名字<br/>
`-res`：分辨率<br/>
`-compset`：需要哪些分量模式组合，如何组合等设置，具体参考手册<br/>
`-mach`：机器名，和前面的配置一致<br/>
进入名为test的case文件夹，修改env_mach_specific，env_mach_pes.xml等文件，需要注意配置用多少个CPU。<br/>
依次执行cesm_setup，check_case，check_input_data这三个脚本。<br/>
执行后缀为build的脚本进行编译，然后执行后缀为submit的脚本进行提交：`${CASE}.build，${CASE}.submit`

## CAM单独编译运行
> 前面总结的是CESM模式的编译运行，但是理论上CESM的每个分量模式都可以单独编译运行。由于笔者主要用过CAM，这里记录CAM的单独编译运行流程。

- ## CAM3
> CAM3不同于目前的CAM5，CAM3是谱模式，同时单独提供一个文件下载。CAM3现在用的较少，而且不能使用太新的并行库，但是CAM3编译较为简单，这里一并记录。

- ### 环境变量设置 
先配置好环境变量：<br/>
`#For CAM3`<br/>
`export CAM_ROOT=/home/qianqf/cam3/cam1`<br/>
`export INC_NETCDF=/share/apps/local/include`<br/>
`export LIB_NETCDF=/share/apps/local/lib`<br/>
`export INC_MPI=/share/apps/openmpi-1.4.5/include`<br/>
`export LIB_MPI=/share/apps/openmpi-1.4.5/lib`<br/>
`export USER_FC=ifort`<br/>
`export USER_CC=icc`<br/>
`export CSMDATA=/home/qianqf/cam3`

- ### 配置
把下面的这些内容写入一个configure.sh文件执行configure配置CAM3：<br/>
`#!/bin/sh`<br/>
`$CAM_ROOT/models/atm/cam/bld/configure -cam_bld /home/qianqf/CAM3/bld -cam_exedir /home/qianqf/CAM3 -spmd -test -debug`<br/>
`cd ./bld`<br/>
`gmake`<br/>
`cd ..`<br/>
`$CAM_ROOT/models/atm/cam/bld/build-namelist -config /home/qianqf/CAM3/bld/config_cache.xml -test`<br/>

- ### 配置namelist
`&camexp`<br/>
`　absems_data            = '/home/qianqf/cam3/atm/cam/rad/abs_ems_factors_fastvx.c030508.nc'`<br/>
`　aeroptics              = '/home/qianqf/cam3/atm/cam/rad/AerosolOptics_c040105.nc'`<br/>
`　bnd_topo               = '/home/qianqf/cam3/atm/cam/topo/topo-from-cami_0000-09-01_64x128_L26_c030918.nc'`<br/>
`　bndtvaer               = '/home/qianqf/cam3/atm/cam/rad/AerosolMass_V_64x128_clim_c031022.nc'`<br/>
`　bndtvo         = '/home/qianqf/cam3/atm/cam/ozone/pcmdio3.r8.64x1_L60_clim_c970515.nc'`<br/>
`　bndtvs         = '/home/qianqf/sst_64X128x_201202modify.nc'`<br/>
`　caseid         = 'camrun2013avg'`<br/>
`　iyear_ad               = 1950`<br/>
`　ncdata         = '/home/qianqf/cam3/atm/cam/inic/gaus/cami_0000-09-01_64x128_L26_c030918.nc'`<br/>
`　start_ymd      = 20000101`<br/>
`　nelapse                = -3650`<br/>
`　nsrest         = 0`<br/>
`　scon           = 1.367E6`<br/>
`　co2vmr         = 3.550e-4`<br/>
`/`<br/>
`&clmexp`<br/>
`　finidat                = '/home/qianqf/cam3/lnd/clm2/inidata_2.1/cam/clmi_0000-09-01_64x128_T42_USGS_c030609.nc'`<br/>
`　fpftcon                = '/home/qianqf/cam3/lnd/clm2/pftdata/pft-physiology'`<br/>
`　fsurdat                = '/home/qianqf/cam3/lnd/clm2/srfdata/cam/clms_64x128_USGS_c030605.nc'`<br/>
`/`

- ### 运行
`./cam < namelist`

- ## CAM5
> CAM5使用的一般流程如下：运行config.sh-->进入bld文件夹下，运行gmake -j2 或者 gmake-->运行namelist.sh-->修改各项参数-->运行CAM5。CAM5的环境变量可以与CESM设置在一起，因为CAM5和CAM3不同，不再单独提供一个包给用户使用。

- ### config.sh配置
`#!/bin/sh`<br/>
`#$camcfg/configure --help`<br/>
`#$camcfg/configure -dyn fv -hgrid 10x15 -nosmp -nospmd -test -ice sice -ocn socn -lnd slnd -rof srof -cam_bld /home/qianqf/camtest/fuck -cc icc -debug -fc ifort -fc_type intel -target_os linux`<br/>
`#$camcfg/configure -dyn fv -hgrid 4x5 -nosmp -spmd -ntasks 16 -test -ice cice -ocn docn -lnd clm -rof rtm -cam_bld /home3_hn/qianqf/CAM/bld -cc mpicc -debug -fc mpif90 -fc_type intel -target_os linux`<br/>
`$camcfg/configure -dyn fv -hgrid 0.9x1.25 -carma none -nosmp -spmd -ntasks 160 -test -cam_bld /home/qianqf/CAM5/try/bld -cc mpicc -debug -fc mpif90 -fc_type intel -target_os linux`

- ### namelist.sh设置namelist
`#!/bin/sh`<br/>
`$camcfg/build-namelist -test -config /home/wjh/CAM5/try/bld/config_cache.xml -ntasks 160`

- ### 运行
`./cam < namelist`

- ## 修改海冰与海温强迫场和制作初始场
海温和海冰的数据都放在不同的文件里，ice_in文件里面的stream_domfilename和stream_fldfilename两个设置分别指向强迫场的数据。文件docn.stream.txt中filepath和filename两个参数也需要修改成对应强迫场的文件位置。
初始场的制作，需要先在atm_in中ncdata参数的下一行增加inithist='6-HOURLY'，和inithist_all=True，然后运行一段时间即可。
