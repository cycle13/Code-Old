---
layout:     post
title:      "10个技巧让开发者的Git水平迅速提升"
subtitle:   "10 git tips"
date:       2017-10-12
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Git
---

# 1. Git自动补全

假使你使用命令行工具运行Git命令，那么每次手动输入各种命令是一件很令人厌烦的事情。

为了解决这个问题，你可以启用Git的自动补全功能，完成这项工作仅需要几分钟。

为了得到这个脚本，在Unix系统下运行以下命令：

```
cd ~
curl https://raw.github.com/git/git/master/contrib/completion/git-completion.bash -o ~/.git-completion.bash
```

然后，添加下面几行到你的 ~/.bash_profile 文件中：

```
if [ -f ~/.git-completion.bash ]; then
. ~/.git-completion.bash
fi
```

尽管早些时候我们已经提到这个，但是强调的不够充分。如果你想使用git的全部功能特性，你绝对应该切换到命令行界面！

# 2. 在 Git 中忽略文件

你是不是很烦那些编译过的文件 (比如 .pyc) 出现在你的 Git 仓库中？或者说你已经受够了已经把它们都加进了 Git 仓库？好了，这有个办法可以让你告诉 Git 忽略掉那些特定的文件和文件夹。只需要创建一个名为 .gitignore 然后列出那些你不希望 Git 跟踪的文件和文件夹。你还可以添加例外，通过使用感叹号(!)。

```
1. *.pyc
2. *.exe
3. my_db_config/
4. 
5. !main.pyc
```

# 3. 是谁弄乱了我的代码？

当事情出错时，先去指责别人是人类的天性之一。如果你的产品服务器挂了，使用git blame命令可以很容易找出罪魁祸首。这个命令可以将文件中的每一行的作者、最新的变更提交和提交时间展示出来。

```
git blame [file_name]
```

![img](/img/in-post/2017-10-12-Ten-git-tips/01.png)

在下面的截图中你可以看到命令是如何在更大的目录中搜寻。

![img](/img/in-post/2017-10-12-Ten-git-tips/02.png)

# 4. 查看仓库历史记录

上一节我们已经学习了如何使用 git log ，不过，这里还有三个你应该知道的选项。

* --oneline- 压缩模式，在每个提交的旁边显示经过精简的提交哈希码和提交信息，以一行显示。
* --graph- 图形模式，使用该选项会在输出的左边绘制一张基于文本格式的历史信息表示图。如果你查看的是单个分支的历史记录的话，该选项无效。
* --all- 显示所有分支的历史记录

把这些选项组合起来之后，输出看起来会像这样：

![img](/img/in-post/2017-10-12-Ten-git-tips/03.png)

# 5. 绝对不要丢失对Commit的跟踪

假设你不小心提交了些你不想要的东西，不得不做一次强制重置来恢复到之前的状态。然后，你意识到在这一过程中你丢失了其它一些信息并且想要把它们找回来，或者至少瞅一眼。这正是git reflog可以做到的。

一个简单的git log命令可以为你展示最后一次commit，以及它的父亲，还有它父亲的父亲等等。而git reflog则列出了head曾经指向过的一系列commit。要明白它们只存在于你本机中；而不是你的版本仓库的一部分，也不包含在push和merge操作中。

如果我运行git log命令，我可以看到一些commit，它们都是我仓库的一部分：

![img](/img/in-post/2017-10-12-Ten-git-tips/04.png)

然而，一个git reflog命令则展示了一次`commit (b1b0ee9&#8211;HEAD@{4})`，它正是我刚才进行强制重置时弄丢的：

![img](/img/in-post/2017-10-12-Ten-git-tips/05.png)

# 6. 暂存文件的部分改动

一般情况下，创建一个基于特性的提交是比较好的做法，意思是每次提交都必须代表一个新特性的产生或者是一个bug的修复。如果你修复了两个bug，或是添加了多个新特性但是却没有提交这些变化会怎样呢？在这种情况下，你可以把这些变化放在一次提交中。但更好的方法是把文件暂存(Stage)然后分别提交。

例如你对一个文件进行了多次修改并且想把他们分别提交。这种情况下，你可以在 add 命令中加上 -p 参数

```
git add -p [file_name]
```

我们来演示一下在 file_name 文件中添加了3行文字，但只想提交第一行和第三行。先看一下 git diff 显示的结果：

![img](/img/in-post/2017-10-12-Ten-git-tips/06.png)

然后再看看在 add 命令中添加 -p 参数是怎样的？

![img](/img/in-post/2017-10-12-Ten-git-tips/07.png)

看上去，Git 假定所有的改变都是针对同一件事情的，因此它把这些都放在了一个块里。你有如下几个选项：

* 输入 y 来暂存该块
* 输入 n 不暂存
* 输入 e 手工编辑该块
* 输入 d 退出或者转到下一个文件
* 输入 s 来分割该块

在我们这个例子中，最终是希望分割成更小的部分，然后有选择的添加或者忽略其中一部分。

![img](/img/in-post/2017-10-12-Ten-git-tips/08.png)

正如你所看到的，我们添加了第一行和第三行而忽略了第二行。之后你可以查看仓库状态之后并进行提交。

![img](/img/in-post/2017-10-12-Ten-git-tips/09.png)

# 7. 压缩多个Commit

当你提交代码进行代码审查时或者创建一次pull request (这在开源项目中经常发生)，你的代码在被接受之前会被要求, 做一些变更。于是你进行了变更，并且直到下一次审查之前你没有被要求进行过变更。在你直到又要进行变更之前，你已经有了一些额外的commit。理想情况下，你可以用rebase命令把多个commit压缩成一个。

```
git rebase -i HEAD~[number_of_commits]
```

如果你想要压缩最后两个commit，你需要运行下列命令。

```
git rebase -i HEAD~2
```

运行该命令时，你会看到一个交互界面，列出了许多commit让你选择哪些需要进行压缩。理想情况下，你选择最后一次commit并把其它老commit都进行压缩。

![img](/img/in-post/2017-10-12-Ten-git-tips/10.png)

然后会要求你为新的commit录入提交信息。这一过程本质上重写了你的commit历史。

![img](/img/in-post/2017-10-12-Ten-git-tips/11.png)

# 8. Stash未提交的更改

你正在修改某个bug或者某个特性，又突然被要求展示你的工作。而你现在所做的工作还不足以提交，这个阶段你还无法进行展示（不能回到更改之前）。在这种情况下， git stash可以帮助你。stash在本质上会取走所有的变更并存储它们为以备将来使用。stash你的变更，你只需简单地运行下面的命令-

```
git stash
```

希望检查stash列表，你可以运行下面的命令：

```
git stash list
```

![img](/img/in-post/2017-10-12-Ten-git-tips/12.png)

如果你想要解除stash并且恢复未提交的变更，你可以进行apply stash:

```
git stash apply
```

在屏幕截图中，你可以看到每个stash都有一个标识符，一个唯一的号码（尽管在这种情况下我们只有一个stash）。如果你只想留有余地进行apply stash，你应该给apply添加特定的标识符：

```
git stash apply stash@{2}
```

![img](/img/in-post/2017-10-12-Ten-git-tips/13.png)

# 9.检查丢失的提交

尽管 reflog 是唯一检查丢失提交的方式。但它不是适应用于大型的仓库。那就是 fsck（文件系统检测）命令登场的时候了。

```
git fsck --lost-found
```

![img](/img/in-post/2017-10-12-Ten-git-tips/14.png)

这里你可以看到丢掉的提交。你可以通过运行 git show [commit_hash] 查看提交之后的改变或者运行git merge [commit_hash] 来恢复到之前的提交。

git fsck 相对reflog是有优势的。比方说你删除一个远程的分支然后关闭仓库。 用fsck 你可以搜索和恢复已删除的远程分支。

# 10. Cherry Pick

我把最优雅的Git命令留到了最后。cherry-pick命令是我目前为止最喜欢的git命令，既是因为它的字面意思，也因为它的功能。

简而言之，cherry-pick就是从不同的分支中捡出一个单独的commit，并把它和你当前的分支合并。如果你以并行方式在处理两个或以上分支，你可能会发现一个在全部分支中都有的bug。如果你在一个分支中解决了它，你可以使用cherry-pick命令把它commit到其它分支上去，而不会弄乱其他的文件或commit。

让我们来设想一个用得着它的场景。我现在有两个分支，并且我想cherry-pick b20fd14: Cleaned junk 这个commit到另一个上面去。

![img](/img/in-post/2017-10-12-Ten-git-tips/15.png)

我切换到想被cherry-pick应用到的这个分支上去，然后运行了如下命令：

```
git cherry-pick [commit_hash]
```

尽管我们这次完成了一次干净的cherry-pick，你也应该意识到这个命令可能会产生冲突。所以用它时请无比小心。

# 总结

说着说着就到了文章的最后，我认为这些技巧会让你的 Git 水平提升到一个新的高度。Git 是最棒的，它几乎能实现你所能想到的事情。因此，要经常挑战自己的Git水平。最后你很有可能会学到新的东西。