---
layout:     post
title:      "Python 数据结构——二叉堆的实现"
subtitle:   "Python binary heap"
date:       2017-10-15
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
    - 算法
---

# 优先队列的二叉堆实现

在前面的章节里我们学习了“先进先出”（FIFO）的数据结构：队列（Queue）。队列有一种变体叫做“优先队列”（Priority Queue）。优先队列的出队（Dequeue）操作和队列一样，都是从队首出队。但在优先队列的内部，元素的次序却是由“优先级”来决定：高优先级的元素排在队首，而低优先级的元素则排在后面。这样，优先队列的入队（Enqueue）操作就比较复杂，需要将元素根据优先级尽量排到队列前面。我们将会发现，对于下一节要学的图算法中的优先队列是很有用的数据结构。

我们很自然地会想到用排序算法和队列的方法来实现优先队列。但是，在列表里插入一个元素的时间复杂度是O(n)，对列表进行排序的时间复杂度是O(nlogn)。我们可以用别的方法来降低时间复杂度。一个实现优先队列的经典方法便是采用二叉堆（Binary Heap）。二叉堆能将优先队列的入队和出队复杂度都保持在O(logn)。

二叉堆的有趣之处在于，其逻辑结构上像二叉树，却是用非嵌套的列表来实现。二叉堆有两种：键值总是最小的排在队首称为“最小堆（min heap）”，反之，键值总是最大的排在队首称为“最大堆（max heap）”。在这一节里我们使用最小堆。

# 二叉堆的操作

二叉堆的基本操作定义如下：

* BinaryHeap()：创建一个空的二叉堆对象
* insert(k)：将新元素加入到堆中
* findMin()：返回堆中的最小项，最小项仍保留在堆中
* delMin()：返回堆中的最小项，同时从堆中删除
* isEmpty()：返回堆是否为空
* size()：返回堆中节点的个数
* buildHeap(list)：从一个包含节点的列表里创建新堆

下面所示代码是二叉堆的示例。可以看到无论我们以哪种顺序把元素添加到堆里，每次都是移除最小的元素。我们接下来要来实现这个过程。

```
from pythonds.trees.binheap import BinHeap
 
bh = BinHeap()
bh.insert(5)
bh.insert(7)
bh.insert(3)
bh.insert(11)
 
print(bh.delMin())
 
print(bh.delMin())
 
print(bh.delMin())
 
print(bh.delMin())
```

# 二叉堆结构性质

为了更好地实现堆，我们采用二叉树。我们必须始终保持二叉树的“平衡”，就要使操作始终保持在对数数量级上。平衡的二叉树根节点的左右子树的子节点个数相同。在堆的实现中，我们采用“完全二叉树”的结构来近似地实现“平衡”。完全二叉树，指每个内部节点树均达到最大值，除了最后一层可以只缺少右边的若干节点。图 1 所示是一个完全二叉树。

![img](/img/in-post/2017-10-15-python-binary-heap/01.png)

图 1：完全二叉树

有意思的是我们用单个列表就能实现完全树。我们不需要使用节点，引用或嵌套列表。因为对于完全二叉树，如果节点在列表中的下标为 p，那么其左子节点下标为 2p，右节点为 2p+1。当我们要找任何节点的父节点时，可以直接使用 python 的整除。如果节点在列表中下标为n，那么父节点下标为n//2.图 2 所示是一个完全二叉树和树的列表表示法。注意父节点与子节点之间 2p 与 2p+1 的关系。完全树的列表表示法结合了完全二叉树的特性，使我们能够使用简单的数学方法高效地遍历一棵完全树。这也使我们能高效实现二叉堆。

# 堆次序的性质

我们在堆里储存元素的方法依赖于堆的次序。所谓堆次序，是指堆中任何一个节点 x，其父节点 p 的键值均小于或等于 x 的键值。图 2 所示是具备堆次序性质的完全二叉树。

![img](/img/in-post/2017-10-15-python-binary-heap/02.png)

图 2：完全树和它的列表表示法

# 二叉堆操作的实现

接下来我们来构造二叉堆。因为可以采用一个列表保存堆的数据，构造函数只需要初始化一个列表和一个currentSize来表示堆当前的大小。Listing 1 所示的是构造二叉堆的 python 代码。注意到二叉堆的heaplist并没有用到，但为了后面代码可以方便地使用整除，我们仍然保留它。

Listing 1

```
class BinHeap:
    def __init__(self):
        self.heapList = [0]
        self.currentSize = 0
```

我们接下来要实现的是insert方法。首先，为了满足“完全二叉树”的性质，新键值应该添加到列表的末尾。然而新键值简单地添加在列表末尾，显然无法满足堆次序。但我们可以通过比较父节点和新加入的元素的方法来重新满足堆次序。如果新加入的元素比父节点要小，可以与父节点互换位置。图 3 所示的是一系列交换操作来使新加入元素“上浮”到正确的位置。

![img](/img/in-post/2017-10-15-python-binary-heap/03.png)

图 3：新节点“上浮”到其正确位置

当我们让一个元素“上浮”时，我们要保证新节点与父节点以及其他兄弟节点之间的堆次序。当然，如果新节点非常小，我们仍然需要将它交换到其他层。事实上，我们需要不断交换，直到到达树的顶端。Listing 2 所示的是“上浮”方法，它把一个新节点“上浮”到其正确位置来满足堆次序。这里很好地体现了我们之前在headlist中没有用到的元素 0 的重要性。这样只需要做简单的整除，将当前节点的下标除以 2，我们就能计算出任何节点的父节点。

在Listing 3 中，我们已经可以写出insert方法的代码。insert里面很大一部分工作是由percUp函数完成的。当树添加新节点时，调用percUp就可以将新节点放到正确的位置上。

Listing 2

```
def percUp(self,i):
    while i // 2 > 0:
      if self.heapList[i] < self.heapList[i // 2]:
         tmp = self.heapList[i // 2]
         self.heapList[i // 2] = self.heapList[i]
         self.heapList[i] = tmp
      i = i // 2
```

Listing 3

```
def insert(self,k):
    self.heapList.append(k)
    self.currentSize = self.currentSize + 1
    self.percUp(self.currentSize)
```

我们已经写好了insert方法，那再来看看delMin方法。堆次序要求根节点是树中最小的元素，因此很容易找到最小项。比较困难的是移走根节点的元素后如何保持堆结构和堆次序，我们可以分两步走。首先，用最后一个节点来代替根节点。移走最后一个节点保持了堆结构的性质。这么简单的替换，还是会破坏堆次序。那么第二步，将新节点“下沉”来恢复堆次序。图 4 所示的是一系列交换操作来使新节点“下沉”到正确的位置。

![img](/img/in-post/2017-10-15-python-binary-heap/04.png)

图 4：替换后的根节点下沉

为了保持堆次序，我们需将新的根节点沿着一条路径“下沉”，直到比两个子节点都小。在选择下沉路径时，如果新根节点比子节点大，那么选择较小的子节点与之交换。Listing 4 所示的是新节点下沉所需的percDown和minChild方法的代码。

Listing 4

```
def percDown(self,i):
    while (i * 2) <= self.currentSize:
        mc = self.minChild(i)
        if self.heapList[i] > self.heapList[mc]:
            tmp = self.heapList[i]
            self.heapList[i] = self.heapList[mc]
            self.heapList[mc] = tmp
        i = mc
 
def minChild(self,i):
    if i * 2 + 1 > self.currentSize:
        return i * 2
    else:
        if self.heapList[i*2] < self.heapList[i*2+1]:
            return i * 2
        else:
            return i * 2 + 1
```

Listing 5 所示的是delMin操作的代码。可以看到比较麻烦的地方由一个辅助函数来处理，即percDown。

Listing 5

```
def delMin(self):
    retval = self.heapList[1]
    self.heapList[1] = self.heapList[self.currentSize]
    self.currentSize = self.currentSize - 1
    self.heapList.pop()
    self.percDown(1)
    return retval
```

关于二叉堆的最后一部分便是找到从无序列表生成一个“堆”的方法。我们首先想到的是，将无序列表中的每个元素依次插入到堆中。对于一个排好序的列表，我们可以用二分搜索找到合适的位置，然后在下一个位置插入这个键值到堆中，时间复杂度为O(logn)。另外插入一个元素到列表中需要将列表的一些其他元素移动，为新节点腾出位置，时间复杂度为O(n)。因此用insert方法的总开销是O(nlogn)。其实我们能直接将整个列表生成堆，将总开销控制在O(n)。Listing 6 所示的是生成堆的操作。

Listing 6

```
def buildHeap(self,alist):
    i = len(alist) // 2
    self.currentSize = len(alist)
    self.heapList = [0] + alist[:]
    while (i > 0):
        self.percDown(i)
        i = i - 1
```

![img](/img/in-post/2017-10-15-python-binary-heap/05.png)

图 5：将列表[ 9, 6, 5, 2, 3]生成一个二叉堆

图 5 所示的是利用buildHeap方法将最开始的树[ 9, 6, 5, 2, 3]中的节点移动到正确的位置时所做的交换操作。尽管我们从树中间开始，然后回溯到根节点，但percDown方法保证了最大子节点总是“下沉”。因为堆是完全二叉树，任何在中间的节点都是叶节点，因此没有子节点。注意，当i=1时，我们从根节点开始下沉，这就需要进行大量的交换操作。可以看到，图 5 最右边的两颗树，首先 9 从根节点的位置移走，移到下一层级之后，percDown进一步检查它此时的子节点，保证它下降到不能再下降为止，即下降到正确的位置。然后进行第二次交换，9 和 3 的交换。由于 9 已经移到了树最底层的层级，便无法进一步交换了。比较一下列表表示法和图 5 所示的树表示法进行的一系列交换还是很有帮助的。

```
i = 2  [0, 9, 5, 6, 2, 3]
i = 1  [0, 9, 2, 6, 5, 3]
i = 0  [0, 2, 3, 6, 5, 9]
```

下列所示的代码是完全二叉堆的实现。

```
class BinHeap:
    def __init__(self):
        self.heapList = [0]
        self.currentSize = 0
 
 
    def percUp(self,i):
        while i // 2 > 0:
          if self.heapList[i] < self.heapList[i // 2]:
             tmp = self.heapList[i // 2]
             self.heapList[i // 2] = self.heapList[i]
             self.heapList[i] = tmp
          i = i // 2
 
    def insert(self,k):
      self.heapList.append(k)
      self.currentSize = self.currentSize + 1
      self.percUp(self.currentSize)
 
    def percDown(self,i):
      while (i * 2) <= self.currentSize:
          mc = self.minChild(i)
          if self.heapList[i] > self.heapList[mc]:
              tmp = self.heapList[i]
              self.heapList[i] = self.heapList[mc]
              self.heapList[mc] = tmp
          i = mc
 
    def minChild(self,i):
      if i * 2 + 1 > self.currentSize:
          return i * 2
      else:
          if self.heapList[i*2] < self.heapList[i*2+1]:
              return i * 2
          else:
              return i * 2 + 1
 
    def delMin(self):
      retval = self.heapList[1]
      self.heapList[1] = self.heapList[self.currentSize]
      self.currentSize = self.currentSize - 1
      self.heapList.pop()
      self.percDown(1)
      return retval
 
    def buildHeap(self,alist):
      i = len(alist) // 2
      self.currentSize = len(alist)
      self.heapList = [0] + alist[:]
      while (i > 0):
          self.percDown(i)
          i = i - 1
 
bh = BinHeap()
bh.buildHeap([9,5,6,2,3])
 
print(bh.delMin())
print(bh.delMin())
print(bh.delMin())
print(bh.delMin())
print(bh.delMin())
```

能在O(n)的开销下能生成二叉堆看起来有点不可思议，其证明超出了本书的范围。但是，要理解用O(n)的开销能生成堆的关键是因为logn因子基于树的高度。而对于buildHeap里的许多操作，树的高度比logn要小。