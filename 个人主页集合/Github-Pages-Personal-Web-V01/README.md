# QQF's Notebook 模板

### [我的笔记在这里](https://qqfraphael.github.io)

### 关于我的改进
- 替换了一些背景，以及删除了一些多说的评论框，将笔记本网站个人化
- 利用MathJax实现了markdown编辑数学公式
- 新增了页脚的一些个人社交网络连接
- 增加了微信二维码的插入，以及点击后跳出二维码
- 在页脚插入了"回到顶部"的选项
- 增加了程序代码单独成块并根据不同语言语法高亮显示功能
- 增加了程序代码行号功能
- 新增了Portfolio，采用了海报画廊的设计风格，能够有点击翻转效果，也有随机洒落海报的效果

后续会新增一些其他功能

### 插入程序代码
在编写markdown时增加下面这样的块就可以插入代码，语法高亮显示同时显示行号，具体细节请戳[code-prettify](https://github.com/google/code-prettify)<br/>
\<pre class="prettyprint lang-py linenums"\><br/>
your code here<br/>
\</pre\><br/>
支持的语言有：<br/>
"bsh", "c", "cc", "cpp", "cs", "csh", "cyc", "cv", "htm", "html",<br/>
"java", "js", "m", "mxml", "perl", "pl", "pm", "py", "rb", "sh",<br/>
"xhtml", "xml", "xsl".

### 新增Portfolio栏目内容
所有内容均存放在portfolio文件夹下。images文件夹中存放需要展示的项目，并按照“序号 项目名.jpg”存放，注意序号和项目名之间有一个空格。点击图片后图片翻转显示具体的说明，这部分说明存放在images文件夹下的data.js文件中，格式参考data.js文件。

## 致谢

1. 这个模板是从这里[https://github.com/Huxpro/huxpro.github.io](https://github.com/Huxpro/huxpro.github.io) fork 的。 感谢这个作者黄玄提供了如此优质的模板！
2. 感谢 Jekyll、Github Pages、Bootstrap、MathJax、code-prettify、多说和markdown!

## PS

由于多说项目已经关闭，所以评论功能暂时关闭。
