---
layout:     post
title:      "WRF参数配置"
subtitle:   "Parameters in WRF"
date:       2016-12-13
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - 天气预报与数值模式
---

这部分参数仅用于由真实大气方案的预处理程序产生的输入数据。当输入数据产生于理想大气试验方案时，这部分参数将会被忽略。对于大多数真实大气方案来说，起止时间的分和秒都应该设为0。常用的小时和秒之间的换算关系有：

3小时＝10800秒；6小时＝21600秒；12小时＝43200秒。

&time_control

run_days

运行的天数

run_hours

运行的小时数

注意：如果模式积分时间大于1天，则可同时设置run_days和_run_hours，也可设置run_hours一个参数。比如：模式运行的总时间长度为36小时，则可设置run_days=1，且run_hours=12，或者设置run_days=0，且run_hours=36。

run_minutes

运行的分钟数

run_seconds

运行的秒数

start_year(max_dom) =2001

四位数字表示的起始年份。

start_month(max_dom) =04

两位数字(01-12)表示的起始月份。

start_day(max_dom) =20

两位数字(01-31)表示的起始天数。

start_hour(max_dom) =12

两位数字(00-23)表示的起始小时数。

start_minute(max_dom) =00

两位数字(00-59)表示的起始分钟数。

start_second (max_dom) =00

两位数字(00-59)表示的起始秒数。

end_year(max_dom) =2001

四位数字表示的终止年份。

end_month(max_dom) = 04

两位数字(01-12)表示的终止月份。

end_day(max_dom) ＝21

两位数字(01-31)表示的终止天数。

end_hour ＝00

两位数字(00-23)表示的终止小时数。

end_minute ＝00

两位数字(00-59)表示的终止分钟数。

end_second ＝00

两位数字(00-59)表示的终止秒数。

说明：起止时间设置也可以用来控制模式的积分的起止。并且，real.exe的时间控制信息是用起止时间参数来设定的。

模式的积分时间可以用run_days、run_hours等来控制，也可以用end_year、end_month等来控制。但前者run_days等优先与后者end_year等。而在real.exe中只用end_year等来控制时间信息。



interval_seconds ＝43200

前处理程序的两次分析时间之间的时间间隔，以秒为单位。也即模式的实时输入数据的时间间隔，一般为输入边界条件的文件的时间间隔。

input_from_file (max_dom) = T

嵌套初始场输入选项。嵌套时，指定嵌套网格是否用不同的初始场文件。

fine_input_stream(max_dom) = 0

选择从嵌套网格中的输入要素场，仅在嵌套网格时有用。0表示选择从子嵌套网格中输入的所有要素场，2表示在子网格嵌套输入场中仅选择由通道2（在注册表中定义）所指定的那些要素场。

history_interval (max_dom) = 60

此参数指定模式结果输出的时间间隔，以分钟为单位。

frames_per_outfile (max_dom) = 1

此参数指定每一个结果文件中保存输出结果的次数，因此可以将模式结果分成多个文件保存，默认值为 10。

restart = F

指定模式运行是否为断点重启方式。

restart_interval = 1440

此参数指定模式断点重启输出的时间间隔，以分钟为单位。

io_form_history = 2

指定模式结果输出的格式, 2为netCDF格式

io_form_restart = 2

指定模式断点重启输出的格式, 2为netCDF格式

io_form_initial = 2

指定模式初始场数据的格式, 2为netCDF格式

io_form_boundary = 2

指定模式边界条件数据的格式, 2为netCDF格式，4为PHD5格式，5为GRIB1格式（目前没有后处理程序），1为二进制格式（目前没有后处理程序）。

debug_level ＝0

此选项指定模式运行时的调试信息输出等级。取值可为 0,50,100,200,300 ，数值越大，调试信息输出就越多，默认值为 0。

auxhist2_outname = "rainfall"

指定模式加密输出文件的文件名，缺省时取值为“auxhist2_d_”。另外，需要指出的是，加密输出变量需要修改注册表文件Registry.EM。

auxhist2_interval = 10

此参数指定模式加密结果输出的时间间隔，以分钟为单位。

io_form_auxhist2 = 2

指定模式加密输出文件的格式, 2为netCDF格式

nocolons ＝ .FALSE.

在输出文件名中是否用下划线“_”代替冒号“:”。



运行3DVAR时需要的额外参数：

write_input ＝ T

指定模式是否输出用于3DVAR的输入数据格式

inputout_interval ＝ 180

此参数指定模式结果输出用于3DVAR的输入数据的时间间隔，以分钟为单位。

input_outname ＝ ‘wrf_3dvar_input_d<domain>_<date>’

指定模式出用于3DVAR的输入数据文件名，缺省时取值为“wrf_3dvar_input_d<domain>_<date>”。

inputout_begin_y = 0

四位数字表示输出3DVAR数据开始年份。

inputout_begin_mo ＝ 0

两位数字表示输出3DVAR数据开始月份。

inputout_begin_d ＝ 0

两位数字表示输出3DVAR数据开始日期。

inputout_begin_h ＝ 3

两位数字表示输出3DVAR数据开始时次。

Inputout_begin_m ＝ 0

两位数字表示输出3DVAR数据开始分钟数。

inputout_begin_s = 0

两位数字表示输出3DVAR数据开始秒数。

inputout_end_y ＝ 0

四位数字表示输出3DVAR数据终止年份。

inputout_end_mo ＝ 0

两位数字表示输出3DVAR数据终止月份。

inputout_end_d ＝ 0

两位数字表示输出3DVAR数据终止日期。

inputout_end_h ＝ 12

两位数字表示输出3DVAR数据终止时次。

Inputout_end_m ＝ 0

两位数字表示输出3DVAR数据终止分钟数。

inputout_end_s ＝ 0

两位数字表示输出3DVAR数据终止秒数。

说明：输出用于3DVAR输入数据的时间控制以上面的默认设置为例，模式将从第3时次到第12时次每180分钟输出一次。

&domains

time_step = 60

积分的时间步长，为整型数，单位为秒。

time_step_fract_num = 0

实数型时间步长的分子部分。

time_step_fract_den = 1

实数型时间步长的分母部分。

说明：如果想以60.3秒作为积分时间步长，那么可以设置time_step=60，time_step_fract_num=3，并且设置time_step_fract_den=10。其中time_step对应与时间步长的整数部分，time_step_fract_num/time_step_fract_den对应于时间步长的小数部分。

max_dom =1

计算区域个数。计算区域默认值为1，如果使用嵌套功能，则max_dom大于1。

s_we(max_dom) ＝1

x方向(西-东方向)的起始格点值 (通常为1).

e_we(max_dom) ＝32

x方向(西-东方向)的终止格点值 (通常为x方向的格点范围)。

s_sn (max_dom) ＝1

y方向(南-北方向)的起始格点值 (通常为1).

e_sn (max_dom) ＝32

y方向(南-北方向)的终止格点值 (通常为y方向的格点范围)。

s_vert (max_dom) ＝1

z方向(垂直方向)的起始格点值。

e_vert (max_dom) ＝31

z方向(垂直方向)的终止格点值，即全垂直eta层的总层数。垂直层数在各嵌套网格中必须保持一致。

num_metgrid_levels ＝18

来自WPS的metgrid的输入数据的垂直层次数。一般为WPS的三维变量的层数加上一层地面量，比如三维量是17层，那么总数应该是17＋1＝18层。

eta_levels ＝1.0, 0.997, … , 0.0

模式的eta层数值，仅用于来自WPS的输入数据。此eta的数值个数要与模式的垂直层数（e_vert）相一致。如果缺省，real程序会自动生成一套eta数值。

force_sfc_in_vinterp ＝1

在垂直插值时，在边界层低层，使用地面量作为模式面量的层数。默认值时只有1层，即最低层使用地面量作为模式面量。

p_top_requested ＝5000

模式的顶部气压，单位为帕。

interp_type ＝1

垂直插值的类型：1，气压线性插值；2，对数气压线性插值

lagrange_order ＝1

垂直插值的精度阶数：1，线性；2，二次

lowest_lev_form_sfc ＝.FALSE.

是否使用地面量作为模式最低层的值（u,v,t,q）。.TRUE.：使用；.FALSE.：利用通常的插值方法插值。

dx (max_dom) ＝10000 (单位为米)

指定x方向的格距。 在真实大气方案中，此参数值必须与输入数据中的x方向格距一致。

dy (max_dom) ＝10000 (单位为米)

指定y方向的格距。通常与x方向格距相同。

ztop (max_dom) ＝10000 (单位为米)

此参数指定模式顶的高度。通常取20000 米。在真实大气方案中，用于高度坐标动力框架模式，此高度值必须与WRF SI的数据或其他输入数据中的高度值相同。在质量坐标动力框架中，此高度值仅用于理想实验方案。

grid_id (max_dom) ＝1

计算区域的编号。一般是从1开始。

parent_id (max_dom) = 0

嵌套网格的上一级网格（母网格）的编号。一般是从0开始。

i_parent_start (max_dom) = 0

嵌套网格的左下角（LLC）在上一级网格（母网格）中x方向的起始位置。

j_parent_start (max_dom) = 0

嵌套网格的左下角（LLC）在上一级网格（母网格）中y方向的起始位置。

parent_grid_ratio(max_dom) = 1

嵌套时，母网格相对于嵌套网格的水平网格比例。在真实大气方案中，此比例必须为奇数；在理想大气方案中，如果将返馈选项feedback设置为0的话，则此比例也可以为偶数。

parent_time_step_ratio (max_dom) = 1

嵌套时，母网格相对于嵌套网格的时间步长比例。

feedback = 1

嵌套时，嵌套网格向母网格得反馈作用。设置为0时，无反馈作用。而反馈作用也只有在母网格和子网格的网格比例(parent_grid_ratio)为奇数时才起作用。

smooth_option = 0

向上一级网格（母网格）反馈的平滑选项，只有设置了反馈选项为1时才起作用的。 0: 不平滑; 1: 1-2-1 平滑; 2: smoothing-desmoothing

移动网格控制参数说明：控制移动嵌套网格的方式有两种：1. 用户指定移动网格，此方式是指嵌套网格的每一次移动都是由用户通过参数来指定；2. 自动移动网格，此方式是指移动嵌套网格在启动后，根据模式的计算状态，自动判断下一次的移动参数。

用户指定移动：编译时需要在ARCHFLAGS选项中添加“－D MOVE_NESTS”来激活。允许的最大移动套网格移动次数为50，不过也可以在源程序frame/module_driver_constants.F进行修改。）

num_moves ＝ 4

移动嵌套网格总移动次数。

move_id ＝ 2,2,2,2,

每一次移动嵌套网格区域编号列表。

move_interval = 60,120,150,180,

每一次移动的启动时间列表，单位为分钟，自模式积分起始时刻算起。

move_cd_x = 1,1,0,-1,

在i方向（即东西方向）每一次相对于父网格移动格点数。

move_cd_y = 1,0,-1,1,

在j方向（即南北方向）每一次相对于父网格移动格点数。

正整数表示顺着i/j值增大的方向，负值表示顺着i/j值减小的方向。0表示不移动。目前移动距离限制只能为一个网格单元。

自动移动：编译时需要在ARCHFLAGS选项中添加“－D MOVE_NESTS”和“-DVORTEX_CENTER”来激活。目前，这些参数是应用中等涡旋追随法（mid-level vortex following algorithm）来确定嵌套网格的移动，还在测试阶段。

vortex_interval = 15

经过多长时间计算一次涡旋的位置，单位为分钟

max_vortex_speed = 40

涡旋的最大移动速度，用于计算新涡旋位置的搜索半径

corral_dist = 8

移动嵌套网格靠近粗网格边界允许的最大网格单元数，此参数也就是规定了移动网格靠近粗网格允许的最大距离。

虽然不同的嵌套网格可以使用不同的物理方案，但必须注意没中方案的使用条件和范围。

&physics

chem_opt

此选项指定是否使用化学过程方案，默认值为 0。

mp_physics (max_dom)

此选项设置微物理过程方案，默认值为 0。目前的有效选择值为：

= 0, 不采用微物理过程方案

= 1, Kessler 方案 (暖雨方案)

= 2, Lin 等的方案 (水汽、雨、雪、云水、冰、冰雹)

= 3, WSM 3类简单冰方案

= 4, WSM 5类方案

= 5, Ferrier(new Eta)微物理方案(水汽、云水)

= 6, WSM 6类冰雹方案

= 8, Thompson 等方案

= 98, NCEP 3类简单冰方案 (水汽、云/冰和雨/雪) （将放弃）

= 99, NCEP 5类方案(水汽、雨、雪、云水和冰)（将放弃）

新添参数：

mp_zero_out = 0,

选用微物理过程时，保证Qv .GE. 0, 以及当其他一些水汽变量小于临界值时，将其设置为0。

= 0, 表示不控制，

= 1, 除了Qv外，所有的其他水汽变量当其小于临界值时，则设置为0

= 2, 确保Qv .GE. 0, 并且所有的其他水汽变量当其小于临界值时，则设置为0 。

mp_zero_out_thresh = 1.e-8

水汽变量（Qv除外）的临界值，低于此值时，则设置为0 (kg/kg)。

ra_lw_physics(max_dom)

此选项指定长波辐射方案，默认值为 0。有效选择值如下：

= 0, 不采用长波辐射方案

= 1, rrtm 方案

= 99, GFDL (Eta) 长波方案 (semi-supported)

ra_sw_physics (max_dom)

此选项指定短波辐射方案，默认值为 0。有效选择值如下：

= 0, 不采用短波辐射方案

= 1, Dudhia 方案

= 2, Goddard 短波方案

= 99, GFDL (Eta) 短波方案 (semi-supported)

radt (max_dom)

此参数指定调用辐散物理方案的时间间隔，默认值为 0, 单位为分钟。通常比较合理的间隔值为30分钟。当网格水平分辨率提高时，则需将间隔时间相应地缩短。建议为水平分辨率的1倍，如dx＝10km，则取10分钟。

nrads (max_dom) =

用于NMM版本WRF。指定调用短波辐散过程的时间间隔，单位为粗网格的时间步数。默认值在Registry.NMM 进行设置，但也可在namelist.input文件中重新赋值。模式会根据此数值计算radt的值。

nradl (max_dom) =

用于NMM版本WRF。指定调用长波辐散过程的时间间隔，单位为粗网格的时间步数。默认值在Registry.NMM 进行设置，但也可在namelist.input文件中重新赋值。模式会根据此数值计算radt的值。

co2tf ＝ 0

CO2 的传输函数参数，用于GFDL 辐射方案

= 0, 从预生成的数据文件中读取CO2 函数数据

= 1, 模式自己生成CO2 函数

sf_sfclay_physics (max_dom)

此选项指定近地面层(surface-layer)方案，默认值为 0。旧参数表中的bl_sfclay_physics ，有效选择值有：

= 0, 不采用近地面层方案

= 1, Monin-Obukhov 方案

= 2, MYJ Monin-Obukhov 方案 (仅用于MYJ 边界层方案)

= 3, NCEP Global Forecast System scheme

sf_surface_physics(max_dom)

此选项指定陆面过程方案，默认值为 0。旧参数表中的bl_surface_physics，有效选择值有：

= 0, 不采用陆面过程方案

= 1, 热量扩散方案

= 2, Noah 陆面过程方案

= 3, RUC 陆面过程方案

bl_pbl_physics (max_dom)

此选项指定边界层方案，默认值为 0。有效选择值有：

= 0, 不采用边界层方案

= 1, YSU 方案

= 2, Eta Mellor-Yamada-Janjic TKE(湍流动能) 方案

= 3, NCEP Global Forecast System scheme

= 99, MRF 方案（将放弃）

bldt (max_dom)

此参数指定调用边界层物理方案的时间间隔，默认值为 0，单位为分钟。0 (推荐值)表示每一个时间步长都调用边界层物理方案。

cu_physics (max_dom)

此选项指定积云参数化方案，默认值为 0。有效选择值有：

= 0, 不采用积云参数化方案

= 1, 浅对流Kain-Fritsch (new Eta)方案

= 2, Betts-Miller-Janjic 方案

= 3, Grell-Devenyi 集合方案

= 4, 简化Arakawa-Schubert 方案

= 99, 老Kain-Fritsch 方案

cudt(max_dom)

此参数设定积云参数化方案的调用时间间隔，默认值为 0, 单位为分钟。 一般的积云参数化方案是每一步都要调用，但如果是用Kain-Fritsch 方案(cu_physics=1)，则可以设cudt=5。

ncnvc (max_dom) =

用于NMM版本WRF。指定调用积云参数化过程的时间间隔，单位为粗网格的时间步数。默认值在Registry.NMM 进行设置，但也可在namelist.input文件中重新赋值。模式会根据此数值计算cudt的值。

ISFFLX - 在使用扰动边界层时有效，即sf_sfclay_physics = 1

此选项指定在选用扰动边界层和陆面物理过程时，是否考虑地面热量和水汽通量，默认值为 1：

1 = 考虑地面通量

0 = 不考虑地面通量

IFSNOW

此选项指定是否考虑雪盖效应。考虑雪盖效应时，必须要有雪盖输入场。默认值为0，只有在利用扰动边界层PBL预报土壤温度是才有效，即sf_surface_physics = 1。

1 = 考虑雪盖效应

0 = 不考虑雪盖效应

ICLOUD

此参数指定辐射光学厚度中是否考虑云的影响，默认值为1。仅当ra_sw_physics = 1 和ra_lw_physics = 1时有效。

1 = 考虑云的影响

0 = 不考虑云的影响

surface_input_source

此参数知道土地利用类型和土壤类型数据的来源格式，默认值为1。

1 = SI/gridgen（由SI的gridgen_model.exe程序产生）

2 = 其他模式产生的GRIB码数据

(VEGCAT/SOILCAT 数据都在由SI产生的wrf_real_input_em 文件中)

num_soil_layers

指定陆面模式中的土壤层数，默认值为5

＝5: 热量扩散方案

= 4: Noah 陆面过程方案

= 6: RUC 陆面过程方案

maxiens

默认值为1，仅用于积云参数化方案中的Grell-Devenyi集合方案

maxens

默认值为3，仅用于积云参数化方案中的Grell-Devenyi集合方案

maxens2

默认值为3，仅用于积云参数化方案中的Grell-Devenyi集合方案

maxens3

默认值为16，仅用于积云参数化方案中的Grell-Devenyi集合方案

ensdim

默认值为144， 仅用于积云参数化方案中的Grell-Devenyi集合方案

说明：以上这些用于Grell-Devenyi方案的默认值，都是一下推荐使用的数值。如果要改变数值，请谨慎修改。

seaice_threshold = 271

海冰温度临界值。当TSK小于此临界值时，如果模式格点是水体，陆面过程选用5层的SLAB方案，则将此模式格点设置为陆地，且为永久性冰体；如果模式格点是水体，陆面过程选用Noah方案，则将此模式格点设置为陆地，且为永久性冰体，并将设置0～3米的TEMPS，以及设置SMOIS和SH2O。

sst_update = 0

时变海温控制参数。0表示不用，1表示使用。如果选择使用时变海温，则real.exe会从wrflowinp_d01文件中读取SST和VEGFRA数据，wrf.exe则会以更新边条件数据相同的时间间隔来更新这些数据。要使用此功能，则在参数列表文件namelist.input的时间控制区还必须包含auxinput5_interval, auxinput5_end_h, 和 auxinput5_inname = "wrflowinp_d<domain>"。

&dynamics

dyn_opt

模式框架配置选项，默认值为 2：

1 = 欧拉高度坐标 （已经放弃）

2 = 欧拉质量坐标

3 = 半拉格朗日 (目前还没完成)

rk_ord

本参数定义Runge-Kutta时间积分方案阶数，默认值为 3：

2 = Runge-Kutta 二阶

3 = Runge-Kutta 三阶 (推荐)

diff_opt

湍流和混合作用选项，默认值为 0:

0 = 没有湍流或者显式空间数值滤波(km_opt将被忽略) (explicit spatial numerical filters)

1 = 老扩散方案, 计算坐标面上二阶扩散项。如果没有指定PBL选项，则用kvdif选项当作垂直扩散系数。通常用于km_opt=1或者4。（在真实大气方案的水平分辨率格距小于10km时，推荐使用1）

2 = 新扩散方案, 计算物理空间(x,y,z)中的混合作用项(应力形式)。用km_opt来指明湍流参数化过程。

km_opt

湍涡系数选项，默认值为1：

1 = 固定不变 (用参数配置第三部分的khdif, kvdif参数值) 与diff_opt=1的区别在于km_opt的水平扩散作用不在模式的zeta面上。因此，只有在没有地形的情况下，这两个选项的作用才是相同的。

2 = 1.5 阶TKE(湍流动能)闭合（3D）

3 = Smagorinsky 一阶闭合

说明：2和3在水平格距大于2km时，不推荐使用。

4 =水平 Smagorinsky 一阶闭合

说明：4在水平格距小于10km时，推荐使用。

damp_opt

顶层抽吸作用标志选项 (当diff_opt = 1时，此选项失效)，默认值为。 同时，必需在参数配置第三部分设置zdamp和dampcoef参数。

0 = 无抽吸作用

1 = 有抽吸作用

w_damping

默认值为0。垂直速度拟制标志选项 (用于实际业务)

0 = 无拟制作用

1 = 有拟制作用

zdamp (max_dom)

只有在damp_opt = 1时，才使用此参数。

此参数设定模式顶部的抽吸厚度。推荐值为5000 米，单位为米。如果选项diff_opt=1，那么zdamp参数无效。

base_temp = 290.,

用于真实数据，欧拉质量坐标方案。基准海平面气温(K)。

base_pres = 10^5

用于真实数据，欧拉质量坐标方案。基准海平面气压(Pa)。勿需改动。

base_lapse = 50.,

用于真实数据，欧拉质量坐标方案。基准气温直减率(K)。勿需改动。

dampcoef (max_dom) (默认值为 0)

只有在damp_opt = 1时，才使用此参数。

此参数指定抽吸系数(dampcoef <= 0.2)，并且与zdamp选项一起配合使用,默认值为0，使用时最好dampcoef <= 0.15。

khdif (max_dom)

此参数设定水平扩散系数(单位为m^2/s)，默认值为 0。使用此参数时，必须设定选项diff_opt = 1或者km_opt = 1。

kvdif (max_dom)

此参数设定垂直扩散系数(单位为m^2/s)，默认值为 0。使用此参数时，必须设定选项diff_opt = 1 或者km_opt = 1。

smdiv(max_dom)

此参数设定辐散抽吸(系数) (通常取为0.1)，默认值为 0。 此参数在时间分裂RK方案中用于选择性地消除声波。

emdiv (max_dom)

此参数用于欧拉质量模式框架中指定额外模态滤波系数(external-mode filter coef)，默认值为0.01。

只用于欧拉质量坐标模式框架的真实大气方案中(通常取为0.01)。

epssm (max_dom)

此参数指定垂直声波的离心时间(time off-centering)，默认值为 0.1。此参数在时间分裂RK方案中用于选择性地消除声波。

non_hydrostatic(max_dom)

模式动力框架参数，指定模式动力框架是否是非静力模式，.true.为非静力，.false.为静力，默认为.False.。

pert_coriolis (max_dom) = .false.,

科氏参数，仅影响扰动风场 (适用于理想方案)

mix_full_fields(max_dom) = .true.,

与diff_opt = 2配合使用。除高分辨率的理想模拟外，推荐取值为".true."，但damp_opt 不能同时为1。当取”.false.”时，表示混合前扣除1维的静态廓线（base-state profile）。

h_mom_adv_order (max_dom)

此选项指定水平动量平流的阶数，默认值为 3。(例如5=5阶，等等) ，有效值为2～6，推荐值为5。

v_mom_adv_order (max_dom)

此选项指定垂直动量平流的阶数，默认值为 3。有效值为2～6，推荐值为3。

h_sca_adv_order (max_dom)

此选项指定水平标量(scalar)平流的阶数，默认值为 3。有效值为2～6，推荐值为5。

v_sca_adv_order (max_dom)

此选项指定垂直标量平流的阶数，默认值为 3。有效值为2～6，推荐值为3。

time_step_sound (max_dom)

每一时间步长中声波的步数(sound steps)。通常为 4，默认值为 10。如果时间步长远大于6×dx（公里），则需增加声波步数.

&bc_control

spec_bdy_width

此参数指定用于边界过渡的格点总行数，默认值为5。此参数只用于真实大气方案。参数的大小至少为spec_zone 和 relax_zone的和。

spec_zone

指定区域(specified zone)的格点数，默认值为 1。指定边条件时起作用。

relax_zone

指定松弛区域的格点数，默认值为4。指定边条件时起作用。

specified (max_dom)

此选项指定是否使用特定边条件，逻辑型, 默认值为 .false.。 特定边条件选项只用于真实大气方案的数值模拟中，并且要求多个时次的边条件数据(文件wrfbdy)。

periodic_x (max_dom)

此选项指定在x方向是否使用周期性边界条件。逻辑型, 默认值为 .false.。 通常只用于理想大气试验方案。

symmetric_xs (max_dom)

此选项指定在x方向的起始点(西边界)是否使用对称性边界条件。逻辑型, 默认值为 .false.。通常只用于理想大气试验方案。

symmetric_xe (max_dom)

此选项指定在x方向的终止点(东边界)是否使用对称性边界条件。逻辑型, 默认值为 .false.。通常只用于理想大气试验方案。

open_xs (max_dom)

此选项指定在x方向的起始点(西边界)是否使用自由边界条件。逻辑型, 默认值为 .false.。通常只用于理想大气试验方案。

open_xe (max_dom)

此选项指定在x方向的终止点(东边界)是否使用自由边界条件。逻辑型, 默认值为 .false.。通常只用于理想大气试验方案。

periodic_y (max_dom)

此选项指定在y方向是否使用周期性边界条件。 逻辑型, 默认值为 .false.。通常只用于理想大气试验方案。

symmetric_ys (max_dom)

此选项指定在y方向的起始点(南边界)是否使用对称性边界条件。逻辑型, 默认值为 .false.。通常只用于理想大气试验方案。

symmetric_ye (max_dom)

此选项指定在y方向的终止点(北边界)是否使用对称性边界条件。逻辑型, 默认值为 .false.。通常只用于理想大气试验方案。

open_ys (max_dom)

此选项指定在y方向的起始点(南边界)是否使用自由边界条件。逻辑型, 默认值为 .false.。通常只用于理想大气试验方案。

open_ye (max_dom)

此选项指定在y方向的终止点(北边界)是否使用自由边界条件。逻辑型, 默认值为 .false.。通常只用于理想大气试验方案。

nested (max_dom)

此选项设定嵌套边条件。逻辑型, 默认值为 .false.。

&namelist_quilt

参数列表的这一部分用于控制MPI异步通讯形式的输入/输出。

nio_tasks_per_group

此参数指定模式需要多少个I/O处理器：

= 0, 不要求单独的I/O处理器。

= n, 如果 n>0, 表明需要n个I/O处理器。

如果指定需要单独的I/O处理器，那么模式要求的总的处理器数目必须大于n+模式计算的处理器数。例如，如果令nio_tasks_per_group = 1，而用户只申请了5个处理器来运行整个系统，那么只有4个处理器用于模式计算，而有1个处理器仅用于输入/输出。

nio_groups

设置为1，目前为预留参数，请勿改动。

tile_sz_x

在共享式内存进程中指定x方向计算的格点数，默认值为0。 如果指定了numtiles，则不需要此参数。

tile_sz_y

在共享式内存进程中指定y方向计算的格点数，默认值为0。 如果指定了numtiles，则不需要此参数。

numtiles

此参数在共享式内存进程中指定每个内存块中的内存片数，默认值为1。

(或者是指定上面tile_sz_x和tile_sz_y两个参数)

nproc_x

区域分解时，指定x方向上的上的线程数，默认值为－1。

nproc_y

区域分解时，指定x方向上的上的线程数，默认值为－1。

-1: 程序自动分解

\>1: 用于分解的数目。


