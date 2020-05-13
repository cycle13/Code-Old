---
layout:     post
title:      "Python 数据结构——二叉搜索树的实现"
subtitle:   "Python binary search tree"
date:       2017-10-19
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
    - 算法
---

# 二叉搜索树

我们已经知道了在一个集合中获取键值对的两种不同的方法。回忆一下这些集合是如何实现ADT（抽象数据类型）MAP的。我们讨论两种ADT MAP的实现方式，基于列表的二分查找和哈希表。在这一节中，我们将要学习二叉搜索树，这是另一种键指向值的Map集合，在这种情况下我们不用考虑元素在树中的实际位置，但要知道使用二叉树来搜索更有效率。

# 搜索树操作

在我们研究这种实现方式之前，让我们回顾一下ADT MAP提供的接口。我们会注意到，这种接口和Python的字典非常相似。

Map() 创建了一个新的空Map集合。

put(key,val) 在Map中增加了一个新的键值对。如果这个键已经在这个Map中了，那么就用新的值来代替旧的值。

get(key) 提供一个键，返回Map中保存的数据，或者返回None。

del 使用del map[key]这条语句从Map中删除键值对。

len() 返回Map中保存的键值对的数目

in 如果所给的键在Map中，使用key in map这条语句返回True。

# 搜索树实现

一个二叉搜索树，如果具有左子树中的键值都小于父节点，而右子树中的键值都大于父节点的属性，我们将这种树称为BST搜索树。如之前所述的，当我们实现Map时，BST方法将引导我们实现这一点。图 1 展示了二叉搜索树的这一特性，显示的键没有关联任何的值。注意这种属性适用于每个父节点和子节点。所有在左子树的键值都小于根节点的键值，所有右子树的键值都大于根节点的键值。

![img](/img/in-post/2017-10-19-python-binary-search-tree/01.png)

图 1：一个简单的二叉搜索树

现在你知道什么是二叉搜索树了，我们再来看如何构造一个二叉搜索树，我们在搜索树中按图 1 显示的节点顺序插入这些键值，图 1 搜索树存在的节点：70,31,93,94,14,23,73。因为 70 是第一个被插入到树的值，它是根节点。接下来，31 小于 70，因此是 70 的左子树。接下来，93 大于 70，因此是 70 的右子树。我们现在填充了该树的两层，所以下一个键值，将会是 31 或者 93 的左子树或右子树。由于 94 大于 70 和 93，就变成了 93 的右子树。同样，14 小于 70 和 31，因此成为了 31 的左子树。23 也小于 31，因此必须是 31 的左子树。然而，它大于 14，所以是 14 的右子树。

为了实现二叉搜索树，我们将使用节点和引用的方法，这类似于我们实现链表和表达式树的过程。因为我们必须能够创建和使用一个空的二叉搜索树，所以我们将使用两个类来实现，第一个类我们称之为 BinarySearchTree，第二个类我们称之为TreeNode。BinarySearchTree类有一个TreeNode类的引用作为二叉搜索树的根，在大多数情况下，外部类定义的外部方法只需检查树是否为空，如果在树上有节点，要求BinarySearchTree类中含有私有方法把根定义为参数。在这种情况下，如果树是空的或者我们想删除树的根，我们就必须采用特殊操作。BinarySearchTree类的构造函数以及一些其他函数的代码如Listing 1 所示。

**Listing 1**

```
class BinarySearchTree:

    def __init__(self):
        self.root = None
        self.size = 0

    def length(self):
        return self.size

    def __len__(self):
        return self.size

    def __iter__(self):
        return self.root.__iter__()
```

TreeNode类提供了许多辅助函数，使得BinarySearchTree类的方法更容易实现过程。如Listing 2 所示，一个树节点的结构，是由这些辅助函数实现的。正如你看到的那样，这些辅助函数可以根据自己的位置来划分一个节点作为左或右孩子和该子节点的类型。TreeNode类非常清楚地跟踪了每个父节点的属性。当我们讨论删除操作的实现时，你将明白为什么这很重要。

对于Listing 2 中的TreeNode实现，另一个有趣的地方是，我们使用Python的可选参数。可选的参数很容易让我们在几种不同的情况下创建一个树节点，有时我们想创建一个新的树节点，即使我们已经有了父节点和子节点。与现有的父节点和子节点一样，我们可以通过父节点和子节点作为参数。有时我们也会创建一个包含键值对的树，我们不会传递父节点或子节点的任何参数。在这种情况下，我们将使用可选参数的默认值。

**Listing 2**

```
class TreeNode:
   def __init__(self,key,val,left=None,right=None,
                                       parent=None):
        self.key = key
        self.payload = val
        self.leftChild = left
        self.rightChild = right
        self.parent = parent

    def hasLeftChild(self):
        return self.leftChild

    def hasRightChild(self):
        return self.rightChild

    def isLeftChild(self):
        return self.parent and self.parent.leftChild == self

    def isRightChild(self):
        return self.parent and self.parent.rightChild == self

    def isRoot(self):
        return not self.parent

    def isLeaf(self):
        return not (self.rightChild or self.leftChild)

    def hasAnyChildren(self):
        return self.rightChild or self.leftChild

    def hasBothChildren(self):
        return self.rightChild and self.leftChild

    def replaceNodeData(self,key,value,lc,rc):
        self.key = key
        self.payload = value
        self.leftChild = lc
        self.rightChild = rc
        if self.hasLeftChild():
            self.leftChild.parent = self
        if self.hasRightChild():
            self.rightChild.parent = self
```

现在，我们拥有了BinarySearchTree和TreeNode类，是时候写一个put方法使我们能够建立二叉搜索树。put方法是BinarySearchTree类的一个方法。这个方法将检查这棵树是否已经有根。如果没有，我们将创建一个新的树节点并把它设置为树的根。如果已经有一个根节点，我们就调用它自己，进行递归，用辅助函数_put按下列算法来搜索树：

* 从树的根节点开始，通过搜索二叉树来比较新的键值和当前节点的键值，如果新的键值小于当前节点，则搜索左子树。如果新的关键大于当前节点，则搜索右子树。
* 当搜索不到左（或右）子树，我们在树中所处的位置就是设置新节点的位置。
* 向树中添加一个节点，创建一个新的TreeNode对象并在这个点的上一个节点中插入这个对象。

Listing 3 显示了在树中插入新节点的Python代码。\_put函数要按照上述的步骤编写递归算法。注意，当一个新的子树插入时，当前节点（CurrentNode）作为父节点传递给新的树。

我们执行插入的一个重要问题是重复的键值不能被正确的处理，我们的树实现了键值的复制，它将在右子树创建一个与原来节点键值相同的新节点。这样做的后果是，新的节点将不会在搜索过程中被发现。我们用一个更好的方式来处理插入重复的键值，旧的值被新键关联的值替换。我们把这个错误的修复，作为练习留给你。

**Listing 3**

```
def put(self,key,val):
    if self.root:
        self._put(key,val,self.root)
    else:
        self.root = TreeNode(key,val)
    self.size = self.size + 1

def _put(self,key,val,currentNode):
    if key < currentNode.key:
        if currentNode.hasLeftChild():
               self._put(key,val,currentNode.leftChild)
        else:
               currentNode.leftChild = TreeNode(key,val,parent=currentNode)
    else:
        if currentNode.hasRightChild():
               self._put(key,val,currentNode.rightChild)
        else:
               currentNode.rightChild = TreeNode(key,val,parent=currentNode)
```

随着put方法的实现，我们可以很容易地通过`__setitem__`方法重载[]作为操作符来调用put方法（参见Listing 4）。这使我们能够编写像myZipTree['Plymouth'] = 55446一样的python语句，这看上去就像Python的字典。

**Listing 4**

```
def __setitem__(self,k,v):
    self.put(k,v)
```

图 2 说明了将新节点插入到一个二叉搜索树的过程。灰色节点显示了插入过程中遍历树节点顺序。

![img](/img/in-post/2017-10-19-python-binary-search-tree/02.png)

图 2: 插入一个键值 = 19 的节点

一旦树被构造，接下来的任务就是为一个给定的键值实现检索。get方法比put方法更容易因为它只需递归搜索树，直到发现不匹配的叶节点或找到一个匹配的键值。当找到一个匹配的键值后，就会返回节点中的值。

Listing 5 显示了get，`_get`和`__getitem__`的代码。用`_get`方法搜索的代码与put方法具有相同的选择左或右子树的逻辑。请注意，`_get`方法返回TreeNode中get的值，`_get`就可以作为一个灵活有效的方式，为BinarySearchTree的其他可能需要使用TreeNode里的数据的方法提供参数。

通过实现`__getitem__`方法，我们可以写一个看起来就像我们访问字典一样的Python语句，而事实上我们只是操作二叉搜索树，例如Z = myziptree ['fargo']。正如你所看到的，`__getitem__`方法都是在调用get。

**Listing 5**

```
def get(self,key):
    if self.root:
        res = self._get(key,self.root)
        if res:
               return res.payload
        else:
               return None
    else:
        return None

def _get(self,key,currentNode):
    if not currentNode:
        return None
    elif currentNode.key == key:
        return currentNode
    elif key < currentNode.key:
        return self._get(key,currentNode.leftChild)
    else:
        return self._get(key,currentNode.rightChild)

def __getitem__(self,key):
    return self.get(key)
```

使用get，我们可以通过写一个BinarySearchTree的`__contains__`方法来实现操作，`__contains__`方法简单地调用了get方法，如果它有返回值就返回True，如果它是None就返回False。如Listing 6 所示。

**Listing 6**

```
def __contains__(self,key):
    if self._get(key,self.root):
        return True
    else:
        return False
```

回顾一下`__contains__`重载的操作符，这允许我们写这样的语句：

```
if 'Northfield' in myZipTree:
    print("oom ya ya"
```

最后，我们把注意力转向二叉搜索树中最具挑战性的方法，删除一个键值（参见Listing 7）。首要任务是要找到搜索树中要删除的节点。如果树有一个以上的节点，我们使用`_get`方法找到需要删除的节点。如果树只有一个节点，这意味着我们要删除树的根，但是我们仍然要检查根的键值是否与要删除的键值匹配。在以上两种情况下，如果没有找到该键，del操作就会报错。

**Listing 7**

```
def delete(self,key):
   if self.size > 1:
      nodeToRemove = self._get(key,self.root)
      if nodeToRemove:
          self.remove(nodeToRemove)
          self.size = self.size-1
      else:
          raise KeyError('Error, key not in tree')
   elif self.size == 1 and self.root.key == key:
      self.root = None
      self.size = self.size - 1
   else:
      raise KeyError('Error, key not in tree')

def __delitem__(self,key):
    self.delete(key)
```

一旦我们找到包含要删除的节点，我们必须考虑三种情况：

1. 要删除的节点没有孩子(见图3).
2. 要删除的节点只有一个孩子(见图4).
3. 要删除的节点有两个孩子(见图5).

第一种情况是最简单的（参见Listing 8）。如果当前节点没有孩子，所有我们需要做的是引用删除该节点并删除父节点的引用。本例的代码显示在如下。

**Listing 8**

```
if currentNode.isLeaf():
    if currentNode == currentNode.parent.leftChild:
        currentNode.parent.leftChild = None
    else:
        currentNode.parent.rightChild = None
```

![img](/img/in-post/2017-10-19-python-binary-search-tree/03.png)

图 3：删除键值为16的节点，这个节点没有孩子

第二种情况只是稍微复杂（参见Listing 9）。如果节点只有一个孩子，那我们可以简单地让孩子替换它父母的位置。此案例的代码如下所示。看到这段代码，就会发现有六种情况要考虑。由于是具有左子树还是右子树的情况，我们只讨论当前节点只有左子树的情况。具体过程如下：

1. 如果当前节点是左子树，那我们只需要更新左子树的引用指向当前节点的父节点，然后更新父节点的左子树引用指向当前节点的左子树。
2. 如果当前节点是右子树，那我们只需要更新右子树的引用指向当前节点的父节点，然后更新父节点的右子树引用指向当前节点的右子树。
3. 如果当前节点没有父节点，它一定是根。这种情况下，我们只需通过调用replaceNodeData方法把键替换为左子树和右子树里的数据。

**Listing 9**

```
else: # this node has one child
   if currentNode.hasLeftChild():
      if currentNode.isLeftChild():
          currentNode.leftChild.parent = currentNode.parent
          currentNode.parent.leftChild = currentNode.leftChild
      elif currentNode.isRightChild():
          currentNode.leftChild.parent = currentNode.parent
          currentNode.parent.rightChild = currentNode.leftChild
      else:
          currentNode.replaceNodeData(currentNode.leftChild.key,
                             currentNode.leftChild.payload,
                             currentNode.leftChild.leftChild,
                             currentNode.leftChild.rightChild)
   else:
      if currentNode.isLeftChild():
          currentNode.rightChild.parent = currentNode.parent
          currentNode.parent.leftChild = currentNode.rightChild
      elif currentNode.isRightChild():
          currentNode.rightChild.parent = currentNode.parent
          currentNode.parent.rightChild = currentNode.rightChild
      else:
          currentNode.replaceNodeData(currentNode.rightChild.key,
                             currentNode.rightChild.payload,
                             currentNode.rightChild.leftChild,
                             currentNode.rightChild.rightChild)
```

![img](/img/in-post/2017-10-19-python-binary-search-tree/04.png)

图 4：删除键值为25的节点，它是只有一个孩子的节点

第三种情况是最难处理的情况（参见Listing 10）。如果一个节点有两个孩子，我们就不可能简单地让其中一个替换节点的位置，我们需要寻找一个节点，用来替换这个将要删除的节点，我们需要的这个节点能够保持现有二叉搜索树的左、右子树的关系。这个节点在树中具有第二大的键值。我们称这个节点为后继节点，我们将一路寻找这个后继节点，后继节点必须保证没有一个以上的孩子，所以既然我们已经知道如何处理这两种情况，我们就可以实现它了。一旦后继结点被删除，我们把它放在树中将被删除的树节点处。

![img](/img/in-post/2017-10-19-python-binary-search-tree/05.png)

图 5：删除键值为5的节点，它有两个孩子节点

第三种情况的处理代码如下所示。注意我们是用findSuccessor和findMin方法来辅助找到后继节点的。要删除后继节点，我们利用spliceOut方法。我们用spliceOut的原因是它能直接使我们想移除的节点，做出正确的变化。我们也可以调用递归删除，但那样我们就会浪费时间反复寻找关键节点。

**Listing 10**

```
elif currentNode.hasBothChildren(): #interior
        succ = currentNode.findSuccessor()
        succ.spliceOut()
        currentNode.key = succ.key
        currentNode.payload = succ.payload
```

找到后继节点的代码如下所示（参见Listing 11），你可以看到TreeNode类的一个方法。这个代码利用二叉搜索树中序遍历的性质，从最小到最大打印出树中的节点。当寻找后继节点时需要考虑三种情况：

1. 如果节点有右子节点，那么后继节点是右子树中最小的关键节点。
2. 如果节点没有右子节点，是其父节点的左子树，那么父节点是后继节点。
3. 如果节点是其父节点的右子节点，而本身无右子节点，那么这个节点的后继节点是其父节点的后继节点，但不包括这个节点。

现在对我们来说首要的问题是从二叉搜索树中删除一个节点。而findSuccessor方法的其他用途，我们将在本章最后的练习中作探讨。

findMin方法是用来找到子树中的最小的节点。你要了解，最小值在任何二叉搜索树中都是树最左边的孩子节点。因此findMin方法只需要简单地追踪左子树，直到找到没有左子树的叶节点。

**Listing 11**

```
def findSuccessor(self):
    succ = None
    if self.hasRightChild():
        succ = self.rightChild.findMin()
    else:
        if self.parent:
               if self.isLeftChild():
                   succ = self.parent
               else:
                   self.parent.rightChild = None
                   succ = self.parent.findSuccessor()
                   self.parent.rightChild = self
    return succ

def findMin(self):
    current = self
    while current.hasLeftChild():
        current = current.leftChild
    return current

def spliceOut(self):
    if self.isLeaf():
        if self.isLeftChild():
               self.parent.leftChild = None
        else:
               self.parent.rightChild = None
    elif self.hasAnyChildren():
        if self.hasLeftChild():
               if self.isLeftChild():
                  self.parent.leftChild = self.leftChild
               else:
                  self.parent.rightChild = self.leftChild
               self.leftChild.parent = self.parent
        else:
               if self.isLeftChild():
                  self.parent.leftChild = self.rightChild
               else:
                  self.parent.rightChild = self.rightChild
               self.rightChild.parent = self.parent
```

我们还需要看看二叉搜索树的最后一个接口。假设我们已经按顺序简单地遍历了子树上所有的键值，就肯定是用字典实现的，就会有疑问：为什么不是树？我们已经知道如何使用中序遍历二叉树的算法，然而，写一个迭代器需要更多的操作，因为每次调用迭代器时，一个迭代器只返回一个节点。

Python提供了一个创建迭代器的非常强大的功能。这个功能就是yield。yield类似于return，返回一个值给调用者。然而，yield也需要额外的步骤来暂停函数的执行，以便下次调用函数时继续执行时做准备。它的功能是创建可迭代的对象，称为生成器。

二叉树迭代器的代码如下所示。仔细观察这些代码：乍一看，你可能会认为代码是非递归的。但是请记住，`__iter__`重写了for x in的操作符进行迭代，所以它确实是递归！因为它是`__iter__`方法在TreeNode类中定义的TreeNode的实例递归。

```
def __iter__(self):
   if self:
      if self.hasLeftChild():
             for elem in self.leftChiLd:
                yield elem
      yield self.key
      if self.hasRightChild():
             for elem in self.rightChild:
                yield elem
```

在此，你可能想下载包含BinarySearchTree和TreeNode类完整代码的文档。

![img](/img/in-post/2017-10-19-python-binary-search-tree/06.png)

# 搜索树分析

通过二叉搜索树实现的完成，我们将对我们已经实现的方法进行一个快速的分析。让我们先看一下put这个方法。对它的执行效果造成限制的是二叉树的高度。回想一下语法阶段，树的高度指的是根和最深的叶节点之间的边的数目。高度作为一种限制因素是因为当我们在树中寻找一个合适的位置插入节点时，我们最多将会对树的每一级做比较。

二叉树的高度可能是多少呢？这个问题的答案取决于键是怎么被添加到树中的。如果键是以一个随机的顺序加到树中的，那么当节点的数目为n时，树的高度大概是 log2n。 这是因为键是随机地散布的，大约有一半的节点会比根节点大，另一半会比它小。记住在二叉树中，根上有一个节点，下一级有两个，再下一级有四个。当级数为d时，这一级的节点 数目为 2d。当h表示树的高度时，一个满二叉树中节点的总数是 2h+1-1。

一个满二叉树中的节点数目和一个平衡二叉树中的左子树和右子树的数目是一样多的。当树中有n个节点时put执行的最差结果的时间复杂度是 O(log2n)。要注意到这与上一段所说的是逆运算的关系。所以 log2n 代表树的高度，这表示把一个新节点插入到一个合适位置前，在搜索中需要比较的次数。


不巧的是，通过插入排序过后的键，建造一个高度为n的搜索树是可能的。图 6 就展示了这种树的一个例子，在这种情形下，这时put方法的时间复杂度为 O(n)。

![img](/img/in-post/2017-10-19-python-binary-search-tree/07.png)

图 6：一个倾斜的二叉搜索树性能会低下

现在你已经理解了put方法的效率会受到树的高度的限制，你可能猜测其他的方法，get，in和del也是受限制的，因为要在树上寻找键，最坏的情形就是一直搜索树最后却找不到键。乍一看del也许看上去更复杂，因为它也许需要在删除操作完成之前一直查找后继节点。但是记得，寻找后继节点的最坏情形也是取决于树的高度，这意味着只需要简单地把工作量加倍，因为加倍是乘以一个常数因子，所以它不会改变最坏情形的复杂度。
