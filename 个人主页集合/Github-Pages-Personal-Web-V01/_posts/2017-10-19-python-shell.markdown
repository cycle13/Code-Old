---
layout:     post
title:      "使用Python脚本在Linux下实现部分Bash Shell的教程"
subtitle:   "Python script to realize shell functions"
date:       2017-10-19
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Python
---

对于Linux用户来说，命令行的名声相当的高。不像其他操作系统，命令行是一个可怕的命题，但是对于Linux社区中那些经验丰富的大牛，命令行却是最值得推荐鼓励使用的。通常，命令行对比图形用户界面，更能提供更优雅和更高效的解决方案。

命令行伴随着Linux社区的成长，UNIX shells，例如 bash和zsh，已经成长为一个强大的工具，也是UNIX shell的重要组成部分。使用bash和其他类似的shells，可以得到一些很有用的功能，例如，管道，文件名通配符和从文件中读取命令，也就是脚本。

让我们在实际操作中来介绍命令行的强大功能吧。每当用户登陆某服务后，他们的用户名都被记录到一个文本文件。例如，我们来看看有多少独立用户曾经使用过该服务。

以下一系列的命令展现了由一个个小的命令串接起来后所实现的强大功能：

```
$ cat names.log | sort | uniq | wc -l
```

管道符号（|）把一个命令的标准输出传送给另外一个命令的标准输入。在这个例子中，把cat names.log的输出传送给sort命令的输入。sort命令是把每一行按字母顺序重新排序。接下来，管道把输出传送至uniq命令，它可以删除重复名字。最后，uniq的输出又传送给wc命令。wc是一个字符计数命令，使用-l参数，可以返回行的数量。管道可以让你把一系列的命令串接在一起。

但是，有时候需求会很复杂，串接命令会变得十分笨重。在这个情况下，shell脚本可以解决这个问题。shell脚本就是一系列的命令，被shell程序所读取，并按顺序执行。Shell脚本同样支持一些编程语言的特性，例如变量，流程控制和数据结构。shell脚步对于经常重复运行的批处理程序非常有用。但是，shell脚本也有一些弱点：

* shell脚本很容易变为复杂的代码，导致开发人员难于阅读和修改它们。
* 通常，它的语法和解释都不是那么灵活，而且不直观。
* 它代码通常不能被其他脚本使用。脚本中的代码重用率很低，并且脚本通常是解决一些很具体的问题。
* 它们一般不支持库特性，例如HTML解释器或者处理HTTP请求库，因为库一般都只出现在流行的语言和脚本语言中。

这些问题通常会导致脚本变得不灵活，并且浪费开发人员大量的时间。而Python语言作为它的替代品，是相当不错的选择。使用python作为shell脚本的替代，通常有很多优势：

* python在主流的linux发行版本中都被默认安装。打开命令行，输入python就可以立刻进入python的世界。这个特性，让它可以成为大多脚本任务的最好选择。
* python非常容易阅读，语法容易理解。它的风格注重编写简约和干净的代码，允许开发人员编写适合shell脚本的风格代码。
* python是一个解释性语言，这意味着，不需要编译。这让python成为最理想的脚本语言。python同时还是读取，演绎，输出的循环风格，这允许开发人员可以快速的通过解释器尝试新的代码。开发人员无需重新编写整个程序，就可以实现自己的一些想法。
* python是一个功能齐全的编程语言。代码重用非常简单，因为python模块可以在脚本中方便的导入和使用。脚本可以轻易的扩展。
* python可以访问优秀的标准库，还有大量的实现多种功能的第三方库。例如解释器和请求库。例如，python的标准库包含时间库，允许我们把时间转换为我们想要的各种格式，而且可以和其他日期做比较。
* python可以是命令链中的一部分。python不能完全代替bash。python程序可以像UNIX风格那样(从标准输入读取，从标准输出中输出)，所以python程序可以实现一些shell命令，例如cat和sort。

让我们基于文章前面提到问题，重新使用python构建。除了已完成的工作，还让我们来看看某个用户登陆系统到底有多少次。uniq命令只是简单的删除重复记录，而没有提示到底这些重复记录重复了多少次。我们使用python脚本替代uniq命令，而且脚本可以作为命令链中的一部分。以下是python程序实现这个功能（在这个例子中，脚本叫做namescount.py）：

```
#!/usr/bin/env python
import sys
if __name__ == "__main__":
   # 初始化一个names的字典，内容为空
   # 字典中为name和出现数量的键值对
   names = {}
   # sys.stdin是一个文件对象。 所有引用于file对象的方法，
   # 都可以应用于sys.stdin.
   for name in sys.stdin.readlines():
           # 每一行都有一个newline字符做结尾
           # 我们需要删除它
           name = name.strip()
           if name in names:
                   names[name] += 1
           else:
                   names[name] = 1
   # 迭代字典,
   # 输出名字，空格，接着是该名字出现的数量
   for name, count in names.iteritems():
           sys.stdout.write("%d\t%s\n" % (count, name))
```

让我们来看看python脚本如何在命令链中起作用的。首先，它从标准输入sys.stdin对象读取数据。所有的输出都写到sys.stdout对象里面，这个对象是python里面的标准输出的实现。然后使用python字典（在其他语言中，叫做哈希表）来保存名字和重复次数的映射。要读取所有用户的登陆次数，只需执行下面的命令：

```
$ cat names.log | python namescount.py
```

这里会输出某用户出现的次数还有他的名字，使用tab作为分隔符。接下来的事情就是，以用户登陆次数的降序顺序输出。这可以在python中实现，但是让我们使用UNIX的命令来实现吧。前面已经提到，使用sort命令可以按字母顺序排序。如果sort命令接收一个-rn参数，那么它就会按照数字的降序方式做排序。因为python脚本输出到标准输出，所以我们可以使用管道链接sort命令，获取该输出：

```
$ cat names.log | python namescount.py | sort -rn
```

这个例子使用了python作为命令链中的一部分。使用python的优势是：

* 可以跟例如cat和sort这样的命令链接在一起。简单的工具（读取文件，给文件按数字排序），可以使用成熟的UNIX命令。这些命令都是一行一行的读取，这意味着这些命令可以兼容大容量的文件，而且它们的效率很高。
* 如果命令链条中某部分很难实现，很清晰，我们可以使用python脚本，这可以让我们做我们想做的，然后减轻链条一下个命令的负担。
* python是一个可重用的模块，虽然这个例子是指定了names，如果你需要处理重复行的其他输入，你可以输出每一行，还有该行的重复次数。让python脚本模块化，这样你就可以把它应用到其他地方。

为了演示python脚本中结合模块和管道风格的强大力量，让我们扩展一下这个问题。让我们来找出使用服务最多的前5位用户。head命令可以让我们指定需要输出的行数。在命令链中加入这个命令：

```
$ cat names.log | python namescount.py | sort -rn | head -n 5
```

这个命令只会列出前5位用户。类似的，获取使用该服务最少的5位用户，你可以使用tail命令，这个命令使用同样的参数。python命令的结果输出到标准输出，这样可以允许你扩展和构建它的功能。

为了演示脚本的模块化特性，我们又来扩展一下问题。该服务同样生成一个以逗号分割的csv的日志文件，其中包含，一个email地址列表，还有该地址对我们服务的评价。如下是其中一个例子：

```
"email@example.com", "This service is great."
```

这个任务是，提供一个途径，来发送一个感谢信息给使用该服务最多的前10位用户。首先，我们需要一个脚本读取csv和输出其中某一个字段。python提供一个标准的csv读取模块。以下的python脚本实现了这个功能：

```
#!/usr/bin/env python
# CSV module that comes with the Python standard library
import csv
import sys
if __name__ == "__main__":
   # CSV模块使用一个reader对象作为输入
   # 在这个例子中，就是 sys.stdin.
   csvfile = csv.reader(sys.stdin)
   # 这个脚本必须接收一个参数，指定列的序号
   # 使用sys.argv获取参数.
   column_number = 0
   if len(sys.argv) > 1:
           column_number = int(sys.argv[1])
   # CSV文件的每一行都是用逗号作为字段的分隔符
   for row in csvfile:
           print row[column_number]
```

这个脚本可以把csv转换并返回参数指定的字段的文本。它使用print代替sys.stout.write，因为print默认使用标准输出最为它的输出文件。

让我们把这个脚步添加到命令链中。新的脚本跟其他命令组合在一起，实现输出评论最多的email地址。（假设.csv 文件名称为emailcomments.csv，新的脚本为csvcolumn.py）

接下来，你需要一个发送邮件的方法，在Python 函数标准库中，你可以导入smtplib 库，这是一个用来连接SMTP服务器并发送邮件的模块。让我们写一个简单的Python脚本，使用这个模块发送一个邮件给每个top 10 的用户。

```
#!/usr/bin/env python
import smtplib
import sys
GMAIL_SMTP_SERVER = "smtp.gmail.com"
GMAIL_SMTP_PORT = 587
GMAIL_EMAIL = "Your Gmail Email Goes Here"
GMAIL_PASSWORD = "Your Gmail Password Goes Here"
def initialize_smtp_server():
   '''
   This function initializes and greets the smtp server.
   It logs in using the provided credentials and returns
   the smtp server object as a result.
   '''
   smtpserver = smtplib.SMTP(GMAIL_SMTP_SERVER, GMAIL_SMTP_PORT)
   smtpserver.ehlo()
   smtpserver.starttls()
   smtpserver.ehlo()
   smtpserver.login(GMAIL_EMAIL, GMAIL_PASSWORD)
   return smtpserver
def send_thank_you_mail(email):
   to_email = email
   from_email = GMAIL_EMAIL
   subj = "Thanks for being an active commenter"
   # The header consists of the To and From and Subject lines
   # separated using a newline character
   header = "To:%s\nFrom:%s\nSubject:%s \n" % (to_email,
           from_email, subj)
   # Hard-coded templates are not best practice.
   msg_body = """
   Hi %s,
   Thank you very much for your repeated comments on our service.
   The interaction is much appreciated.
   Thank You.""" % email
   content = header + "\n" + msg_body
   smtpserver = initialize_smtp_server()
   smtpserver.sendmail(from_email, to_email, content)
   smtpserver.close()
if __name__ == "__main__":
   # for every line of input.
   for email in sys.stdin.readlines():
           send_thank_you_mail(email)
```

这个python脚本能够连接任何的SMTP服务器，不管是在本地还是远程。为便于使用，我使用了Gmail的SMTP服务器，正常情况下，应该提供你连接Gmail的密码口令，这个脚本使用了smtp库中的函数发送邮件。再一次证明使用Python脚本的强大之处，类似SMTP这样的交互操作使用python来写的话是比较简单易读的。相同的shell脚本的话，可能是比较复杂并且像SMTP这样的库是基本没有的。

为了发送电子邮件给评论频率最高的前十名用户，首先必须单独得到电子邮件列的内容。要取出某一列，在Linux中你可以使用cut命令。在下面的例子中，命令是在两个单独的串。为了便于使用，我写输出到一个临时文件，其中可以加载到第二串命令中。这只是让过程更具可读性（Python发送邮件脚本简称为sendemail.py）：

```
$ cat emailcomments.csv | python csvcolumn.py |
↪python namescount.py | sort -rn > /tmp/comment_freq
$ cat /tmp/comment_freq | head -n 10 | cut -f2 |
↪python sendemail.py
```

这表明Python作为一种实用工具如bash命令链的真正威力。编写的脚本从标准输入接受 数据并且将任何输出写入到标准输出，允许开发者串起这些命令， 链中的这些快速，简单的命令以及Python程序。这种只为一个目的设计小程序的哲学非常适用于这里所使用的命令流方式。

通常在命令行中使用的Python脚本，当他们运行某个命令时，参数由用户来选择。例如，head命令取得一个-n的参数标志和它后面的数字，然后只打印这个数字大小的行数。Python脚本的每一个参数都是通过sys.argv数组提供，可在import sys后来访问。下面的代码显示了如何使用单个词语作为参数。此程序是一个简单的加法器，它有两个数字参数，将它们相加，并打印输出给用户。然而，这种命令行参数使用方式是非常基础的。这也是很容易出错误的 ——例如，输入两个字符串，如hello和world，这个命令，你会一开始就得到错误：

```
#!/usr/bin/env python
import sys
if __name__ == "__main__":
   # The first argument of sys.argv is always the filename,
   # meaning that the length of system arguments will be
   # more than one, when command-line arguments exist.
   if len(sys.argv) > 2:
           num1 = long(sys.argv[1])
           num2 = long(sys.argv[2])
   else:
           print "This command takes two arguments and adds them"
           print "Less than two arguments given."
           sys.exit(1)
   print "%s" % str(num1 + num2)
```

庆幸的是，Python有很多处理有关命令行参数的模块。我个人比较喜欢OptionParser。OptionParser是标准库提供的optparse模块的一部分。OptionParser允许你对命令行参数做一系列非常有用的操作。

* 如果没有提供具体的参数，可以指定默认的参数
* 它支持参数标志(显示或不显示）和参数值（-n 10000)。
* 它支持传递参数的不同格式——例如，有差别的-n=100000和-n 100000。

我们来用OptionParser来改进sending-mail脚本。原来的脚本有很多的变量硬编码的地方，比如SMTP细节和用户的登录凭据。在下面提供的代码，在这些变量是用来传递命令行参数：

```
#!/usr/bin/env python
import smtplib
import sys
from optparse import OptionParser
def initialize_smtp_server(smtpserver, smtpport, email, pwd):
   '''
   This function initializes and greets the SMTP server.
   It logs in using the provided credentials and returns the
   SMTP server object as a result.
   '''
   smtpserver = smtplib.SMTP(smtpserver, smtpport)
   smtpserver.ehlo()
   smtpserver.starttls()
   smtpserver.ehlo()
   smtpserver.login(email, pwd)
   return smtpserver
def send_thank_you_mail(email, smtpserver):
   to_email = email
   from_email = GMAIL_EMAIL
   subj = "Thanks for being an active commenter"
   # The header consists of the To and From and Subject lines
   # separated using a newline character.
   header = "To:%s\nFrom:%s\nSubject:%s \n" % (to_email,
           from_email, subj)
   # Hard-coded templates are not best practice.
   msg_body = """
   Hi %s,
   Thank you very much for your repeated comments on our service.
   The interaction is much appreciated.
   Thank You.""" % email
   content = header + "\n" + msg_body
   smtpserver.sendmail(from_email, to_email, content)
if __name__ == "__main__":
   usage = "usage: %prog [options]"
   parser = OptionParser(usage=usage)
   parser.add_option("--email", dest="email",
           help="email to login to smtp server")
   parser.add_option("--pwd", dest="pwd",
           help="password to login to smtp server")
   parser.add_option("--smtp-server", dest="smtpserver",
           help="smtp server url", default="smtp.gmail.com")
   parser.add_option("--smtp-port", dest="smtpserverport",
           help="smtp server port", default=587)
   options, args = parser.parse_args()
   if not (options.email or options.pwd):
           parser.error("Must provide both an email and a password")
   smtpserver = initialize_smtp_server(options.stmpserver,
           options.smtpserverport, options.email, options.pwd)
   # for every line of input.
   for email in sys.stdin.readlines():
           send_thank_you_mail(email, smtpserver)
   smtpserver.close()
```

这个脚本显示OptionParser 的作用。它提供了一个简单、易于使用的接口给命令行参数， 允许你为每个命令行选项定义某些属性。它还允许你指定默认值。如果没有给出某些参数，它可以给你报出特定错误。

现在你学到了多少？并不是使用一个python脚本替代所有的bash命令，我们更推荐让python完成其中某些困难的任务。这需要更多的模块化和重用的脚本，还要好好利用python的强大功能。

使用stdin作为文件对象，这可以允许python读取输入，这个输入是由管道传输其他命令的输出给它的，而把输出输出到stout，可以允许python把信息传递到管道系统的下一环节。结合这些功能，可以实现强大的程序。在这里提到的例子，就是要实现一个处理服务的日志文件。

在实际应用中，我最近在处理一个GB级别的CSV文件，我需要使用python脚本转换一个包含插入数据的SQL命令。了解我需要处理的文件，并在一个表中处理这些数据，脚本需要23个小时来执行并生成20GB的SQL文件。使用文章提到的python编程风格的优势在于，我们不需要把这个文件读取到内存中。这意味着整个20GB+的文件可以一行一行的处理。而且我们更清晰的分解每一个步骤（读取，排序，维护和输出）为一些逻辑步骤。还有我们得到这些命令的保障，其中这些命令都是UNIX类型的环境的核心工具，它们十分高效和稳定，可以帮助我们构建稳定安全的程序。

另外一个优点在于，我们不需要硬编码文件名。这样可以使得程序更灵活，只需传递一个参数。例如，如果脚本在某个文件在20000中断了，我们不需要重新运行脚本，我们可以使用tail来指定失败的行数，来让脚本在这个位置继续运行。

python在shell中的应用范围很广，不局限于本文所述，例如os模块和subprocess模块。os模块是一个标准库，可以执行很多操作系统级别的操作，例如列出目录的结构，文件的统计信息，还有一个优秀的os.path子模块，可以处理规范目录路径。subprocess模块允许python程序运行系统命令和其他高级命令，例如，上文提到的使用python代码和spawned进程之间的管道处理。如果你需要编写python的shell脚本，这些库都值得去研究的。
