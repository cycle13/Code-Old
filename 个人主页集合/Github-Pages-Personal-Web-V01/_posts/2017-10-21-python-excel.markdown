---
layout:     post
title:      "用 Python 创建 Excel 高级工作表"
subtitle:   "Python Excel"
date:       2017-10-21
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

![img](/img/in-post/2017-10-21-python-excel/01.png)

# 引言

我已写过多篇文章介绍如何使用 Python 和 Pandas 操作数据得到有用的 Excel 表格。依我的经验，不管 Python 工具有多么强大，有时候仍然需要利用更高阶的 Excel 特性来传递信息或者进一步分析数据。本文会逐步解释如何通过下列方法改善基于 Excel 的输出数据：

* 用 XlsxWriter 添加 Excel 数据表
* 在 Excel 文件中插入自定义 VBA
* 用 COM 合并 Excel 工作簿

# Excel 表

在前文中，我讨论了如何无缝衔接 Pandas 和 XlsxWriter 来格式化并表达数据，比 Pandas 的 to_excel() 更为复杂。

最近有一个项目，我要给一个简单的数据表添加更多格式，发现如果用 XlsxWriter 这一切都变得很有用而且非常简单。我推荐阅读 XlsxWriter 文档来了解所有选项的背景和细节。

这个例子中，我会使用以前用过的销售数据做样例。该数据将会呈现一段时间内针对各式各样客户的销售情况。我们来汇总一下数据，看看每个客户的购买量以及所有客户的平均购买量是多少：

```
import pandas as pd

sales_df = pd.read_excel('https://github.com/chris1610/pbpython/blob/master/data/sample-salesv3.xlsx?raw=true')
sales_summary = sales_df.groupby(['name'])['ext price'].agg(['sum', 'mean'])
# Reset the index for consistency when saving in Excel
sales_summary.reset_index(inplace=True)
writer = pd.ExcelWriter('sales_summary.xlsx', engine='xlsxwriter')
sales_summary.to_excel(writer, 'summary', index=False)
writer.save()
```

标准的 Excel 输出应该是这个样子：

![img](/img/in-post/2017-10-21-python-excel/02.png)

有用但却普通。

若要将其转换为真正的 Excel 数据表，只要用 XlsxWriter 中的 add_table 函数就好了，非常简单。我一般会写一个 format_excel 函数来保证格式的统一。这种格式化函数一般是这样子的：

```
def format_excel(writer):
    """ Add Excel specific formatting to the workbook
    """
    # Get the workbook and the summary sheet so we can add the formatting
    workbook = writer.book
    worksheet = writer.sheets['summary']
    # Add currency formatting and apply it
    money_fmt = workbook.add_format({'num_format': 42, 'align': 'center'})
    worksheet.set_column('A:A', 20)
    worksheet.set_column('B:C', 15, money_fmt)
    worksheet.add_table('A1:C22', {'columns': [{'header': 'account',
                                                'total_string': 'Total'},
                                               {'header': 'Total Sales',
                                                'total_function': 'sum'},
                                               {'header': 'Average Sales',
                                                'total_function': 'average'}],
                                   'autofilter': False,
                                   'total_row': True,
                                   'style': 'Table Style Medium 20'})
```

用这个函数也非常简单：

```
sales_df = pd.read_excel('https://github.com/chris1610/pbpython/blob/master/data/sample-salesv3.xlsx?raw=true')
sales_summary = sales_df.groupby(['name'])['ext price'].agg(['sum', 'mean'])
# Reset the index for consistency when saving in Excel
sales_summary.reset_index(inplace=True)
writer = pd.ExcelWriter('sales_summary.xlsx', engine='xlsxwriter')
sales_summary.to_excel(writer, 'summary', index=False)
format_excel(writer)
writer.save()
```

改进后的全新输出格式应该是这个样子：

![img](/img/in-post/2017-10-21-python-excel/03.png)

用 Excel 数据表给数据添加总数和其他统计信息是种相当不错的方法。数据表还有便捷工具可以格式化输出得到较好的展示效果。我鼓励你通读 XlsxWriter 文档学习和数据表格式化相关的所有选项。

可以参考 GitHub 上的完整脚本。

# 给 Excel 添加 VBA

我最近创建了一个交互的 Excel 工作簿，用的就是我常在博客中介绍的工具。我想给最终的文件加一小段 VBA 但是不知道该怎么做。幸运地是 XlsxWriter 可以从已有的文件提取 VBA 保存到独立的二进制文件然后将其插入到其他文件中。VBA 宏文档的操作非常清晰明了，这里还是给出一个快速示例。

用 vba_extract.py （XlsxWriter 中就有）来从已有 Excel 文件提取 VBA：

```
vba_extract.py source_file.xlsm
Extracted vbaProject.bin
```

利用类似的代码处理上面的例子，这里演示如何将 VBA 插入 Excel 的输出文件中：

```
import pandas as pd

sales_df = pd.read_excel('https://github.com/chris1610/pbpython/blob/master/data/sample-salesv3.xlsx?raw=true')
sales_summary = sales_df.groupby(['name'])['ext price'].agg(['sum', 'mean'])
# Reset the index for consistency when saving in Excel
sales_summary.reset_index(inplace=True)
writer = pd.ExcelWriter('sales_summary.xlsx', engine='xlsxwriter')
sales_summary.to_excel(writer, 'summary', index=False)
workbook = writer.book
workbook.add_vba_project('vbaProject.bin')
writer.save()
```

敏锐的读者会发现输出文件的拓展名是 .XLSX，但是 Excel 只会执行拓展名为 .XLSM 的文件的 VBA 代码。

不幸的是，如果你尝试像这样将其保存为 XLSM 文件：

```
writer = pd.ExcelWriter('sales_summary.xlsm', engine='xlsxwriter')
```

会报错：

```
ValueError: Invalid extension for engine 'xlsxwriter': 'xlsm'
```

第一种解决方法是用 os.rename 来重命名文件，另外一种更简单的方法是将想要的文件名赋给文件名属性。

```
writer = pd.ExcelWriter('sales_summary.xlsx', engine='xlsxwriter')
sales_summary.to_excel(writer, 'summary', index=False)
workbook = writer.book
workbook.filename = 'sales_summary.xlsm'
workbook.add_vba_project('vbaProject.bin')
writer.save()
```

这个方法或许有点折腾，却是解决这个问题的最简方法。尽管有小小的不方便，这仍然不失为一种强大的特性，可以用 Python 脚本构建健壮的基于 Excel 的解决方案。

# 用 COM 复制 Excel 工作簿

可以用 XlsxWriter 从零开始创建一个 Excel 文件，但是并不能从已有的工作簿复制数据然后引入到一个新文件中。这时最佳方案是用 win32com 实现自动化操作。这一方法的缺点是必须在 Windows 操作系统上使用 win32com，但是如果要合并两个文件，至少有这个选择。

我用这个方法的一个主要原因是，有一些表有复杂的格式或结构，用 Excel 修改非常简单，却难以用 XlsxWriter 进行编程操作。这时一个方案是创建一个「模板」文件，然后将用 Python 完成的工作簿合并进去。

下面的例子基于 Stack Overflow 上的一个回答。这段代码的目的是，从一个标准的「指导」表复制内容到我们用 Pandas 创建的 sales_summary 文件中。

```
from win32com.client import DispatchEx

excel = DispatchEx('Excel.Application')
excel.Visible = False
workbook_1 = excel.Workbooks.Open(r'C:fullpathtosales_summary.xlsx')
workbook_2 = excel.Workbooks.Open(r'C:fullpathtosales_template.xlsx')
workbook_2.Worksheets("Instructions").Move(Before=workbook_1.Worksheets("summary"))
workbook_1.SaveAs(r'C:fullpathtosales_summary_complete.xlsx')
excel.Application.Quit()
del excel
```

这段代码有一些值得注意的点：

* 需要安装 pywin32 – 我推荐使用 anaconda 这个 Python 发行版
* 务必使用 Excel 文件的绝对路径
* 保存新文件时，Excel 可能会弹窗问你是否要覆盖旧文件。你需要在脚本中妥善处理。

我个人觉得用 win32com 必须小心翼翼，所以我尽量少用。但是它仍不失为一个好用的工具，值得放入你的代码工具库。

# 总结

和其他工具一样，Excel 若被滥用会带来一些非常难以维护的工作簿。但是，正因为 Excel 可能会是个麻烦，你必须认识到，在你的工作场景下，何时才应该使用 Excel。Excel 仍会在商业软件生态系统中占据支配地位。本文应该可以帮助你进一步提高能力，更好地用 Python 和 Pandas 开发基于 Excel 的解决方案。
