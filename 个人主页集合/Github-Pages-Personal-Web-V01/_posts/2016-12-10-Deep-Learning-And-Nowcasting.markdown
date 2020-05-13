---
layout:     post
title:      "深度学习与短临预报"
subtitle:   "Deep learning and now casting"
date:       2016-12-10
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - 机器学习
    - 天气预报与数值模式
    - 我的笔记
---



## 背景

随着大气科学与计算机科学技术的发展，数值天气预报成为最重要的天气预报手段。利用基于大气动力学方程的数值天气预报模式，可以提供短至几小时，最长至两周左右的天气预报。然而，数值模式在目前的水平下有一个明显的缺陷——无论多好的数值模式，都无法提供令人满意的精细化降水短临预报。这种缺陷很大程度上来源于数值模式中两种和云相关的参数化方案（积云对流参数化方案和云微物理参数化方案）的不确定性和不完善。

气象学家设计出一种方法来取代数值模式进行短临预报——雷达外推。雷达外推方法较为简单，虽然是一种唯象论的方法，但是在短临预报中往往能够取得比数值模式更好的效果。最早的雷达外推是不考虑任何数学物理方面的基础直接外推，但显然这种方式并不合理，效果也不是最佳。气象学家后来发展出一种更加符合数学物理的新的雷达外推方法。这种雷达外推的基本原理是基于下面这个公式：
$$ 
\dfrac{dP}{dt}=\dfrac{\partial{P}}{\partial{t}}+\vec{V}\cdot\nabla{P} 
$$
这个公式是流体力学中的一个基本公式，右边表示的是随体导数，左边第一项表示的是局地导数，第二项是平流项。在雷达外推技术中，有两个基本的概念——Euler persistence和Lagrange persistence。Euler persistence就是假设局地导数为零，即所有的平流最终都会生消（随体导数的一种意义是降水的生消）；而Lagrange persistence则是假设生消项（随体导数）为零，即所有的降水全部由平流产生。气象学家已经基于这两种概念设计出算法实现了雷达外推，但是显然这两种方案都有明显的缺陷——假设为零的项显然在实际情形下不应当为零。因而气象学家又设计出一种新的方法：refined Lagrange persistence，可以充分利用上面这个公式。这种方法的核心是估计生消项的大小。合理的估计生消项的大小，可以使得外推的有效时长延长，同时获得更高的预报精度。传统的进行生消项估计的一种方法是利用upstream semi-Lagrangian scheme。但是无论如何，这些传统的数值方法都不能很好地建立起生消项和现有流场之间这种复杂的关系，只能利用平流，插值等等方式进行一个粗略的估算。

人工神经网络的技术可以有效地拟合这种无法通过数学公式描述的复杂的关系。通过训练一个或者多个神经网络，我们可以获得一个生消项和流场之间的良好的表达式（尽管我们不知道表达式的数学形式是什么，但它存在于训练完成的一个或多个神经网络中），从而提升雷达外推的精度。同时，利用神经网络对雷达外推生消项的处理，也对机器学习领域有一定的启示：利用机器学习不仅仅可以处理图像学习规律，更可以处理微分方程。

本文主要介绍一种算法的思想，可以将神经网络应用到雷达外推中，提升短临预报的精度。


## 光流法

目前业务上使用的0-2 小时对流临近预报主要是基于雷达数据的雷暴识别追踪和自动外推预报技术。自动外推技术主要包括两类：交叉相关法(TREC)和和单体质心法。单体质心法是将雷暴视为三维单体进行识别、分析、追踪，对雷暴进行拟合外推来做临近预报。交叉相关法利用求雷达回波最优空间的相关，在二维区域内通过计算连续时次雷达回波不同区域的最优空间相关系数，来确定回波在过去的移动矢量特征，建立不同时次雷达回波的最佳拟合关系，从而达到追踪一定区域内雷达回波在过去的移动特征，然后通过这些回波移动特征来外推回波未来的位置和形状。使用交叉相关算法的主要优势在于它的计算方法比较简单。与单体质心法只能用于对流降水系统相比，交叉相关法既可以跟踪对流降水系统，也可以追踪层状云降水。因此，交叉相关法在气象业务部门得到广泛使用。但对局地生成及强度和形状随时间变化很快的降水回波，交叉相关法给出的运动矢量场的质量降低，跟踪失败的情况会显著增加。目前有一种新方法——光流法可以克服上述的问题。光流法是计算机视觉领域中的重要方法，最早由Gibson提出。与传统的交叉相关法相比，光流法立足于变化，而不是选定不变特征再跟踪不变特征移动的方式。目前光流法已经在雷达外推领域得到应用并取得较好的效果。下面介绍光流法的基本原理与统一框架。

光流法的基本方程也是前文给出的公式：
$$ 
\dfrac{dP}{dt}=\dfrac{\partial{P}}{\partial{t}}+\vec{V}\cdot\nabla{P} 
$$
在这里P代表灰度。假设灰度守恒，那么方程变为：
$$
\dfrac{\partial{P}}{\partial{t}}+\vec{V}\cdot\nabla{P}=0
$$
上式中的两项可以根据实际图片中相邻像素的值计算得到。从而可以估算出光流法的误差：
$$
e_1=(\dfrac{\partial{P}}{\partial{t}}+\vec{V}\cdot\nabla{P})^2
$$
上述误差应该取得极小值。但是上述方程只有一个，但是要求得的未知量却有两个，因而方程是不适定的，需要再增加条件才能求解。
对于光滑变化的光流，将其速度梯度范数作为光滑性测度，定义其分量平方和为光滑性测度误差：
$$
e_2=\sum_{i=1}^{n}|\nabla\vec{V}|^2=(\dfrac{\partial{u}}{\partial{x}})^2+（\dfrac{\partial{u}}{\partial{y}})^2+（\dfrac{\partial{v}}{\partial{x}})^2+（\dfrac{\partial{v}}{\partial{y}})^2
$$
对于平滑变化的光流运动，此项也应当取最小值。当然，这项作为$$e_1$$项的约束项，也可以采用不同的假设，建立不同的约束方程。
求解$$e_1$$和$$e_2$$构成的方程，可以采用拉格朗日乘数法。首先构造泛函
$$L=\int \int e_1+\lambda{e_2}dxdy$$
然后根据变分法将其转化为等价的欧拉-拉格朗日方程组，离散化后经过若干次松弛迭代可以求解。

通过光流法得到的风场，仅仅是一种局地风场:
![img](/img/in-post/2016-12-10-Deep-Learning-And-Nowcasting/01.png)
显然，完成雷达外推是远远不够的，我们需要进行风场的补全。补全主要是一种多尺度分析的想法，但是最终待补全的区域补全后的风场，其方向一般是系统整体移动方向：
![img](/img/in-post/2016-12-10-Deep-Learning-And-Nowcasting/02.png)

## 神经网络
在运用光流法得到局地风场并补全后，可以开始构建神经网络并训练它。我们在这里构造两个神经网络，分别用于估算生消项和进一步优化补全后的风场。
对于估算生消项，我们先假设预报的局地降水由平流提供（Lagrange persistence），然后将预报降水和观测降水进行比较，从而可以估算出生消项。用数学公式可以表达为：
$$
\dfrac{dP}{dt}=F(-\nabla\cdot\vec{V},P_{obs})
$$
通过大量图片训练神经网络，可以得到生消项的一种一般化形式$$F$$。
对于风场的进一步优化处理。我们可以划分不同的空间尺度，然后将不同尺度补全的风场和生消项输入给神经网络，从而优化最终输出的风场。
在建立起两个神经网络之后，我们的预报形式可以变为：
$$
\dfrac{\partial{P}}{\partial{t}}=\dfrac{dP}{dt}-\sum_{i=1}^{n}\alpha_{i}\vec{V_{i}}\cdot\nabla{P}
$$
其中$$\alpha_{i}\vec{V_{i}}$$表示不同尺度风场的一种线性组合。
这样我们的预报模型的基本框架已经建立起来了，但是很显然，我们在计算的初始时刻会遇到生消项无法估算的问题，这里我们可以采用数值模式中隐式格式的思想进行处理。

在预报初始时刻，我们一次读入两张图片，记$$P_{obs}^{t=-1}$$和$$P_{obs}^{t=0}$$。我们的预报起始时刻为$$t=0$$。这样，我们可以先从$$t=-1$$时刻预报两步，从$$t=0$$时刻预报一步，得到$$P_{fst}^{t=-1->1}$$和$$P_{fst}^{t=0->1}$$，然后利用前文所述的估算生消项的形式得到预报起始时刻估算的生消项。由于雷达图像一般是6分钟一帧，发现降水系统后的第一张图舍去也没有太大影响。

现在，我们的雷达外推预报系统已经构建起来了。通过这种方式，可以实现数值模式做不到的高时空分辨率（我们国家雷达分辨率为1km，6分钟一次）降水预报，而且效果也不错哦~
![img](/img/in-post/2016-12-10-Deep-Learning-And-Nowcasting/03.png)

## 不足
本文描述的方法有一些不足之处。由于是唯象论方法，除了可以用于可预报性的研究，只能用于预报，而且不能深究其中的物理机理。此外，由于风场的处理在雷达外推里至关重要，有时候风场没处理好，就会导致原本由平流引起的降水会被神经网络误认为是生消。另外，雷达外推的有效时限不长，一般是30分钟内比较好。


## 参考文献

1.Scale-Dependence of the Predictability of Precipitation from Continental Radar Images.Part I Description of the Methodology

2.Scale Dependence of the Predictability of Precipitation from Continental Radar Images.Part II Probability Forecasts

3.Predictability of Precipitation from Continental Radar Images. Part III Operational Nowcasting Implementation (MAPLE)

4.Predictability of Precipitation from Continental Radar Images. Part IV Limits to Prediction

5.Can Lagrangian Extrapolation of Radar Fields Be Used for Precipitation Nowcasting over Complex Alpine Orography

6.Accuracy of Multiply-Upstream, Semi-Lagrangian Advective Schemes

7.光流法及其在临近预报中的应用

8.雷暴与强对流临近天气预报技术进展

9.Three-dimensional storm motion detection by conventional weather radar

10.Nowcasting thunderstorms: A status report

11.交叉相关外推算法的改进及其在对流临近预报中的应用

12.Automatic cell detection and tracking

13.Thunderstorm identification, tracking, analysis, and nowcasting: A radar-based methodology

14.光流法在强对流天气临近预报中的应用

15.光流法及其在气象领域里的应用
