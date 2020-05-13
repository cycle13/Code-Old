---
layout:     post
title:      "eps格式图像处理"
subtitle:   "eps format files"
date:       2017-05-30
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Linux系统运维与服务器管理
---

convert -density XXX   ***.eps ***.png

我们在处理eps图时常常会遇到如下两个问题：

1. 把eps图插入word文件或者ppt文件，显示非常难看，尽管打印成pdf格式或者直接在打印机上打印出来效果很好。在ppt上显示很难看，这就非常令人不爽了，因为我们就是希望显示得要漂亮一点。

2. 有的eps非常大，有时候往往达到几十兆。这时候把这些eps图插入文章中，通过latex编译出来后，文件也变得很大，碰到有大图的页面，图形显示非常缓慢， 让人无法忍受。

为了解决这两个问题，需要对eps图进行再处理。其中的一步就是首先将图形转变成png格式或者jpg格式，同时保持尽可能高的分辨率。这可以通过以下命令来实现：

`gs -r300 -dEPSCrop -dTextAlphaBits=4 -sDEVICE=png16m -sOutputFile=fig.png -dBATCH -dNOPAUSE fig.eps`

其中-r指定分辨率，-r300指的是在每英寸中像素的数目。-dEPSCrop表示在处理过程中仅保留eps图中bounding box范围内的部分。-dTextAlphaBits=4使得文字有光滑好看的边沿。

将这样得到的png文件直接插入ppt，会比你用截图工具得到的图再插入ppt要鲜亮好看得多。这就解决了上述的第一个问题。

接下来，如果你的linux系统里安装了ImageMagick（其实我发现Ubuntu里已经默认安装了，如果没有安装，直接用新立得软件包管理器搜索安装也很快，redhat也默认已经安装了），一定会有convert命令。这个命令可以实现不同图形格式之间的转换。下面我们就用这个命令将png格式转换成eps格式。

`convert fig.png eps3:fig.eps`

这里的eps3表示用Level 3，Level指的是postscript所采用的compatibility level。Level 1效果最差而且文件很大，Level 2支持从jpg产生eps文件，产生的文件也很小。Level 3包含了对Zlib压缩的支持，也可以从png产生eps文件。通常来说，Level 3产生的文件最小，Level 2兼容性最好。

如果要用Level 2，则需要先把png文件转换成jpg文件。

`convert -quality 80 fig.png fig.jpg`

这里-quality指定quality factor，取值范围为0-100。

然后用如下命令来得到Level 2的eps图：

`convert fig.jpg eps2:fig.eps`

因此要得到比较理想的结果，可以通过调节-quality和-r两个参数来实现（运行如下几个命令的时候）。

`gs -r300 -dEPSCrop -dTextAlphaBits=4 -sDEVICE=png16m -sOutputFile=fig.png -dBATCH -dNOPAUSE fig.eps`

`convert -quality 80 fig.png fig.jpg`

`convert fig.jpg eps2:fig.eps`

为了让转换能自动完成，我们可以编辑一个shell脚本文件xheps包含下面的行：

`gs -r300 -dEPSCrop -dTextAlphaBits=4 -sDEVICE=png16m -sOutputFile=fig.png -dBATCH -dNOPAUSE 1` 

`convert -quality 80 fig.png fig.jpg` 

`convert fig.jpg eps2:new_1`

`rm fig.png fig.jpg`

并加上可执行属性（chmod +x xheps），然后放到`/usr/local/bin`下，这样转换的时候运行如下命令：

`xheps figure1.eps`

就会得到new_figure1.eps文件。如果figure1.eps文件很大，那么这个new_figure1.eps将会只有原来的文件的1/10到1/60，取决于具体的图形文件。

我发现，通过这样产生的eps文件，直接插入word和ppt文件，也能很清晰的显示了，不会看上去有很多毛刺了。

此外，在由ps转换成pdf的过程中，图像部分往往感觉很阴暗，这是因为在绝大多数的转换软件中，都采用了DCTEncode选项，这会损失很多信息。如果用 FlateEncode则要好得多。

为了能在转换后的pdf中的图形有较高的质量，可以在转换为pdf的过程中指定用FlateEncode选项或者用具有高质量因子的DCTEncode选项。这可以直接在原来的ps文件注释部分（也就是以%开头的行）结束的地方插入相应行进行设置：

（1） 用具有高质量因子的DCTEncode选项，插入下列行：

```
systemdict /setdistillerparams known {
<< /ColorACSImageDict << /QFactor 0.15 /Blend 1 /ColorTransform 1 /HSamples [1 1 1 1] /VSamples [1 1 1 1] >>
>> setdistillerparams
     } if
```

这里/QFactor后面的0.15设定质量因子。0.15就相当于Acrobat distiller中的"Maximum Quality" mode。这里QFactor要设成一个比较小的数。

（2）用FlateEncode选项，插入下列行：

```
     systemdict /setdistillerparams known {
<< /AutoFilterColorImages false /ColorImageFilter /FlateEncode >> setdistillerparams
     } if
```

用这种方式转换后的pdf文件，将和你原来的eps图看上去完全相同。