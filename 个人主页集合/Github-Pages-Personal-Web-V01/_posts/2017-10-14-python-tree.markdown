---
layout:     post
title:      "Python 数据结构——树的实现"
subtitle:   "Python tree"
date:       2017-10-14
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
    - 算法
---

# “嵌套列表”表示树

在用嵌套列表表示树时，我们使用 Python 的列表来编写这些函数。虽然把界面写成列表的一系列方法与我们已实现其他的抽象数据类型有些不同，但这样做比较有意思，因为它为我们提供一个简单、可以直接查看的递归数据结构。在列表实现树时，我们将存储根节点作为列表的第一个元素的值。列表的第二个元素的本身是一个表示左子树的列表。这个列表的第三个元素表示在右子树的另一个列表。为了说明这个存储结构，让我们来看一个例子。图 1 展示出一个简单的树以及相应的列表实现。

![img](/img/in-post/2017-10-14-python-tree/01.png)

图 1: 简单树

```
myTree = ['a',   #root
      ['b',  #left subtree
       ['d' [], []],
       ['e' [], []] ],
      ['c',  #right subtree
       ['f' [], []],
       [] ]
     ]
```

请注意，我们可以使用索引来访问列表的子树。树的根是myTree[0]，根的左子树是myTree[1]，和右子树是myTree[2]。下面的代码说明了如何用列表创建简单树。一旦树被构建，我们可以访问根和左、右子树。嵌套列表法一个非常好的特性就是子树的结构与树相同，本身是递归的。子树具有根节点和两个表示叶节点的空列表。列表的另一个优点是它容易扩展到多叉树。在树不仅仅是一个二叉树的情况下，另一个子树只是另一个列表。

```
myTree = ['a', ['b', ['d',[],[]], ['e',[],[]] ], ['c', ['f',[],[]], []] ]
print(myTree)
print('left subtree = ', myTree[1])
print('root = ', myTree[0])
print('right subtree = ', myTree[2])
```

让我们定义一些函数，使我们很容易像使用列表一样操作树。请注意，我们不会去定义一个二叉树类。我们将编写的函数将只是操作列表使之类似于树。

```
def BinaryTree(r):
    return [r, [], []]
```

该二叉树只是构建一个根节点和两个空子节点的列表。左子树添加到树的根，我们需要插入一个新的列表到根列表的第二个位置。我们必须注意，如果列表中已经有值在第二个位置，我们需要跟踪它，将新节点插入树中作为其直接的左子节点。Listing 1 显示了插入左子节点。

## Listing 1

```
def insertLeft(root,newBranch):
    t = root.pop(1)
    if len(t) > 1:
        root.insert(1,[newBranch,t,[]])
    else:
        root.insert(1,[newBranch, [], []])
    return root
```

请注意，插入一个左子节点，我们首先获取对应于当前左子节点的列表（可能是空的）。然后，我们添加新的左子节点，将原来的左子节点作为新节点的左子节点。这使我们能够将新节点插入到树中的任何位置。对于insertRight的代码类似于insertLeft，如Listing 2 中。

## Listing 2

```
def insertRight(root,newBranch):
    t = root.pop(2)
    if len(t) > 1:
        root.insert(2,[newBranch,[],t])
    else:
        root.insert(2,[newBranch,[],[]])
    return root
```

为了完善树的实现（参见Listing3），让我们写几个用于获取和设置根值的函数，以及获得左边或右边子树的函数。

## Listing 3


```
def getRootVal(root):
    return root[0]

def setRootVal(root,newVal):
    root[0] = newVal

def getLeftChild(root):
    return root[1]

def getRightChild(root):
    return root[2]
```

以下是完整的嵌套列表表示树的代码

```
def BinaryTree(r):
    return [r, [], []]

def insertLeft(root,newBranch):
    t = root.pop(1)
    if len(t) > 1:
        root.insert(1,[newBranch,t,[]])
    else:
        root.insert(1,[newBranch, [], []])
    return root

def insertRight(root,newBranch):
    t = root.pop(2)
    if len(t) > 1:
        root.insert(2,[newBranch,[],t])
    else:
        root.insert(2,[newBranch,[],[]])
    return root

def getRootVal(root):
    return root[0]

def setRootVal(root,newVal):
    root[0] = newVal

def getLeftChild(root):
    return root[1]

def getRightChild(root):
    return root[2]

r = BinaryTree(3)
insertLeft(r,4)
insertLeft(r,5)
insertRight(r,6)
insertRight(r,7)
l = getLeftChild(r)
print(l)

setRootVal(l,9)
print(r)
insertLeft(l,11)
print(r)
print(getRightChild(getRightChild(r)))
```

# 节点和引用

我们第二种表示树的方式——节点和引用。在这种情况下，我们将定义具有根，以及左右子树属性的类。由于这种表示更紧密地结合了面向对象的方式，我们将继续使用这种表示完成本章的其余部分。

使用节点和引用，我们认为该树的结构类似于图 2 所示。

![img](/img/in-post/2017-10-14-python-tree/02.png)

图 2：使用节点和引用表示简单树

我们将开始用简单的节点和引用的类定义如Listing 4 所示。重要的是要记住这种表示的是左右子树引用的是其他二叉树的实例。例如，当我们插入一个新的左子节点到树上时，我们创建了二叉树的另一个实例，修改了根节点的self leftChild使之指向新的树。

## Listing 4

```
class BinaryTree:
    def __init__(self,rootObj):
        self.key = rootObj
        self.leftChild = None
        self.rightChild = None
```

注意Listing 4 中，构造函数需要得到一些类型的对象存储在根中。就像你可以在列表中存储你喜欢的任何一种类型，树的根对象可以指向任何一种类型。对于我们之前的例子中，我们将存储节点设为根值的名称。使用节点和引用来表示图 2 中的树，我们将创建二叉树类的 6 个实例。

接下来让我们看一下我们需要构建的根节点以外的函数。为了添加左子节点，我们将创建一个新的二叉树，并设置根的左属性以指向这个新对象。insertLeft的代码Listing 5 所示。

## Listing 5

```
def insertLeft(self,newNode):
    if self.leftChild == None:
        self.leftChild = BinaryTree(newNode)
    else:
        t = BinaryTree(newNode)
        t.leftChild = self.leftChild
        self.leftChild = t
```

我们必须考虑两种情况进行插入。第一种情况是，没有左子节点。当没有左子节点时，将新节点添加即可。第二种情况的特征是，当前存在左子节点。在第二种情况下，我们插入一个节点并将之前的子节点降一级。第二种情况是由else语句在Listing 5的第 4 行进行处理。

对于insertRight的代码必须考虑一个对称的情况。要么没有右子节点，要么我们必须插入根和现有的右子节点之间。插入代码Listing 6 所示。


## Listing 6

```
def insertRight(self,newNode):
    if self.rightChild == None:
        self.rightChild = BinaryTree(newNode)
    else:
        t = BinaryTree(newNode)
        t.rightChild = self.rightChild
        self.rightChild = t
```

为了完成一个简单的二叉树数据结构的定义，我们写出访问（参见Listing 7）左右子节点和根值的方法。

## Listing 7

```
def getRightChild(self):
    return self.rightChild

def getLeftChild(self):
    return self.leftChild

def setRootVal(self,obj):
    self.key = obj

def getRootVal(self):
    return self.key
```

既然我们已经有了所有创建和操作二叉树的方法，让我们再进一步检查它的结构。让我们把每一个节点比作一个简单的树的根，并添加节点 B 和 C 作为子节点。 下面的代码就是创建树，并存储一些键值，为左右子节点赋值。注意，左右子节点和根都是同一个二叉树类的不同对象。正如我们之前树的定义中说的，我们能够把一个二叉树的任何子节点当成二叉树来做处理。

```
class BinaryTree:
    def __init__(self,rootObj):
        self.key = rootObj
        self.leftChild = None
        self.rightChild = None

    def insertLeft(self,newNode):
        if self.leftChild == None:
            self.leftChild = BinaryTree(newNode)
        else:
            t = BinaryTree(newNode)
            t.leftChild = self.leftChild
            self.leftChild = t

    def insertRight(self,newNode):
        if self.rightChild == None:
            self.rightChild = BinaryTree(newNode)
        else:
            t = BinaryTree(newNode)
            t.rightChild = self.rightChild
            self.rightChild = t

    def getRightChild(self):
        return self.rightChild

    def getLeftChild(self):
        return self.leftChild

    def setRootVal(self,obj):
        self.key = obj

    def getRootVal(self):
        return self.key


r = BinaryTree('a')
print(r.getRootVal())
print(r.getLeftChild())
r.insertLeft('b')
print(r.getLeftChild())
print(r.getLeftChild().getRootVal())
r.insertRight('c')
print(r.getRightChild())
print(r.getRightChild().getRootVal())
r.getRightChild().setRootVal('hello')
print(r.getRightChild().getRootVal())
```
