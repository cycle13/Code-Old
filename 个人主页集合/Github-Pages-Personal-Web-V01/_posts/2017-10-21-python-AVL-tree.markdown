---
layout:     post
title:      "Python数据结构——AVL树"
subtitle:   "Python AVL tree"
date:       2017-10-21
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
    - 算法
---

# 平衡二叉搜索树

在上一节中我们讨论了建立一个二叉搜索树。我们知道，当树变得不平衡时get和put操作会使二叉搜索树的性能降低到O(n)。在这一节中我们将看到一种特殊的二叉搜索树，它可以自动进行调整，以确保树随时都保持平衡。这种树被称为AVL树，命名源于其发明者：G.M. Adelson-Velskii 和 E.M. Landis。

AVL树实现抽象数据类型Map就像一个普通的二叉搜索树，唯一不同的是这棵树的工作方式。为实现我们的AVL树我们需要在树中的每个节点加入一个平衡因子并跟踪其变化情况。我们通过比较每个节点的左右子树的高度完成比较。更正式地讲，我们定义一个节点的平衡因子为左子树和右子树的高度之差。

balanceFactor=height(leftSubTree)−height(rightSubTree)

利用以上对平衡因子的定义，如果平衡因子大于零，我们称子树“左重”(left-heavy)。如果平衡因子小于零，那么子树“右重”(right-heavy)。如果平衡因子为零，则树是完全平衡的。为实现AVL树，目的是得到一棵平衡的树，我们定义平衡因子如果是 -1，0 或 1，那么这棵树是平衡的。一旦树中节点的平衡因子超出了这个范围，我们需要有一个把树恢复平衡的过程。图 1 是一个不平衡的“右重”树的例子，其中每个节点都标注了平衡因子。

![img](/img/in-post/2017-10-21-python-AVL-tree/07.png)

图 1：一棵标注了平衡因子的不平衡的右重树

# AVL树性能

在我们继续进行之前让我们看看引入这个新的平衡因子的结果。我们的要求是，确保树上的平衡因子始终为 -1，0 或 1。我们可以通过对键的操作得到更好的时间复杂度。首先，我们要思考如何利用这个平衡条件去改变最坏情况下的树。有两种可能性需要考虑，左重树和右重树。如果我们考虑树的高度为 0，1，2 和 3，图 2 举出了在新规则下可能出现的最不平衡的左重树的例子。

![img](/img/in-post/2017-10-21-python-AVL-tree/08.png)

图 2：最坏情况下的左重AVL树

让我们看看树上的节点的总数。我们看到一棵高度为 0 的树有 1 个节点，一个高度为 1 的树有 1 + 1 = 2 个节点，一个高度为 2 的树有 1 + 1 + 2 = 4 个节点，一棵高度为 3 的树有 1 + 2 + 4 = 7 个节点。概括起来，高度为h的树的节点数（$$N_h$$）为：

$$N_h=1+N_h−1+N_h−2$$

可能你很熟悉这个公式，因为它和斐波那契序列非常相似。我们可以利用这个公式通过树中的节点的数目推导出一个AVL树的高度。在我们的印象中，斐波那契数列与斐波那契数的关系为：

$$
F_0 = 0 \\ F_1 = 1 \\ F_i = F_{i-1} + F_{i-2}  text{ for all } i ge 2
$$


数学中一个重要的结果是，随着斐波那契序列的数字越来越大，Fi / Fi−1 越来越接近于黄金比例 ：

$$\Phi=frac1+sqrt52$$

如果你想看到上式的推导过程你可以查阅相关的数学资料。我们简单地将这个方程 Fi 近似为：

$$
Fi=Φ^i/sqrt5
$$

如果利用这种近似我们可以将 $$N_h$$ 的方程改写为：

$$
N_h = F_{h+2} - 1, h ge 1
$$

通过黄金比例近似代替斐波那契数列的项我们可以得到：

$$
N_h = frac{Φ^{h+2}}{sqrt{5}} - 1
$$

如果我们整理这些方程的项，并且两边都以 2 为底取对数，然后求解h，则可以导出：

$$
log{N_h+1} = (H+2)log{Φ} - frac{1}{2} log{5} \\ h = frac{log{N_h+1} - 2 log{Φ} + frac{1}{2} log{5}}{log{Φ}} \\ h = 1.44 log{N_h}
$$

这个推导过程告诉我们，在任何时候我们的AVL树的高度等于树中节点数以 2 为底的对数的常数（1.44）倍。这对我们搜索AVL树来说是好消息因为它限制了搜索的复杂度到O(logN)。

# 实现

既然，我们已经证明，保持 AVL 树的平衡将会使性能得到很大的提升，那我们看看如何在程序中向树插入一个新的键值。因为所有的新键是作为叶节点插入树的，而新叶子的平衡因子为零，所以我们对新插入的节点不作调整。不过一旦有新叶子的插入我们必须更新其父节点的平衡因子。新叶子会如何影响父节点的平衡因子取决于叶节点是左子节点还是右子节点。如果新节点是右子节点，父节点的平衡因子减 1。如果新节点是左子节点，父节点的平衡因子将加 1。这种关系可以递归地应用于新节点的前两个节点，并有可能影响到之前的每一个甚至是根节点。由于这是一个递归的过程，我们看看更新平衡因子的两个基本条件：

* 递归调用已到达树的根。

* 父节点的平衡因子已调整为零。一旦子树平衡因子为零，那么父节点的平衡因子不会发生改变。


我们将实现 AVL 树的子类BinarySearchTree。首先，我们将重写_put方法，并写一个新的辅助方法updateBalance。这些方法如Listing 1 所示。除了第 7 行和第 13 行对 updateBalance的调用，你会注意到_put和简单的二叉搜索树的定义完全相同。

Listing 1

```
def _put(self,key,val,currentNode):
    if key < currentNode.key:
        if currentNode.hasLeftChild():
                self._put(key,val,currentNode.leftChild)
        else:
                currentNode.leftChild = TreeNode(key,val,parent=currentNode)
                self.updateBalance(currentNode.leftChild)
    else:
        if currentNode.hasRightChild():
                self._put(key,val,currentNode.rightChild)
        else:
                currentNode.rightChild = TreeNode(key,val,parent=currentNode)
                self.updateBalance(currentNode.rightChild)

def updateBalance(self,node):
    if node.balanceFactor > 1 or node.balanceFactor < -1:
        self.rebalance(node)
        return
    if node.parent != None:
        if node.isLeftChild():
                node.parent.balanceFactor += 1
        elif node.isRightChild():
                node.parent.balanceFactor -= 1

        if node.parent.balanceFactor != 0:
                self.updateBalance(node.parent)
```

updateBalance方法完成了大部分功能，实现了我们刚提到的递归过程。这个再平衡方法首先检查当前节点是否完全不平衡，以至于需要重新平衡（第 16 行）。如果当前节点需要再平衡，那么只需要对当前节点进行再平衡，而不需要进一步更新父节点。如果当前节点不需要再平衡，那么父节点的平衡因子就需要调整。如果父节点的平衡因子不为零， 算法通过父节点递归调用updateBalance方法继续递归到树的根。

当对一棵树进行再平衡是必要的，我们该怎么做呢？高效的再平衡是使 AVL 树能够很好地执行而不牺牲性能的关键。为了让 AVL 树恢复平衡，我们会在树上执行一个或多个“旋转”
（rotation）。

为了了解什么是旋转，让我们看一个很简单的例子。思考一下图 3 的左边的树。这棵树是不平衡的，平衡因子为 -2。为了让这棵树平衡我们将根的子树节点 A 进行左旋转。


![img](/img/in-post/2017-10-21-python-AVL-tree/01.png)


图 3：使用左旋转变换不平衡树

执行左旋转我们需要做到以下几点：

* 使右节点(B)成为子树的根。
* 移动旧的根节点(A)到新根的左节点。
* 如果新根(B)原来有左节点，那么让原来B的左节点成为新根左节点(A)的右节点。

注：由于新根(B)是 A 的右节点，在这种情况下，移动后的 A 的右节点一定是空的。我们不用多想就可以给移动后的 A 直接添加右节点。

虽然这个过程概念上看起来简单，但实现时的细节有点棘手，因为要保持二叉搜索树的所有性质，必须以绝对正确的顺序把节点移来移去。此外，我们需要确保更新了所有的父节点。
让我们看一个稍微复杂的树来说明右旋转。图 4 的左侧展现了一棵“左重”的树，根的平衡因子为 2。执行一个正确的右旋转，我们需要做到以下几点：

* 使左节点(C)成为子树的根。
* 移动旧根(E)到新根的右节点。
* 如果新根(C)原来有右节点(D)，那么让 D 成为新根右节点(E)的左节点。

注：由于新根(C)是 E 的左节点，移动后的 E 的左节点一定为空。这时可以直接给移动后的 E 添加左节点。

![img](/img/in-post/2017-10-21-python-AVL-tree/02.png)

图 4：使用右旋转变换不平衡树

现在你已经明白了旋转的过程，了解了旋转的方法，让我们看看代码。Listing 2 同时显示了右旋转和左旋转的代码。在第 2 行，我们创建一个临时变量来跟踪新的子树的根。正如我们之前所说的新的根是旧根的右节点。现在，右节点已经存储在这个临时变量中。我们将旧根的右节点替换为新根的左节点。

下一步是调整两个节点的父指针。如果newRoot原来有左节点，左节点的新父节点变成旧根。新根的父节点将成为旧根的父节点。如果旧根是整个树的根，那么我们必须让整棵树的根指向这个新的根。如果旧根是左节点，那么我们改变左节点的父节点到一个新的根；否则，我们改变右节点的父节点到一个新的根（第 10-13 行）。最后我们设置的旧根的父节点成为新的根。这里有很多复杂的中间过程，所以建议你一边看函数的代码，一边看图 3。rotateRight方法和rotateLeft是对称的，所以请自行研究rotateRight的代码。

Listing 2

```
def rotateLeft(self,rotRoot):
    newRoot = rotRoot.rightChild
    rotRoot.rightChild = newRoot.leftChild
    if newRoot.leftChild != None:
        newRoot.leftChild.parent = rotRoot
    newRoot.parent = rotRoot.parent
    if rotRoot.isRoot():
        self.root = newRoot
    else:
        if rotRoot.isLeftChild():
                rotRoot.parent.leftChild = newRoot
        else:
            rotRoot.parent.rightChild = newRoot
    newRoot.leftChild = rotRoot
    rotRoot.parent = newRoot
    rotRoot.balanceFactor = rotRoot.balanceFactor + 1 - min(newRoot.balanceFactor, 0)
    newRoot.balanceFactor = newRoot.balanceFactor + 1 + max(rotRoot.balanceFactor, 0)
```

最后，第 16-17 行需要解释一下。这两行我们更新了旧根和新根的平衡因子。因为其他操作都是移动整个子树，被移动的子树内的节点的平衡因子不受旋转的影响。但我们如何在没有重新计算新的子树的高度的情况下更新平衡因子？下面的推导将让你明白，这些代码都是正确的。

![img](/img/in-post/2017-10-21-python-AVL-tree/03.png)

图 5：左旋转

图5显示了一个左旋转。B 和 D 是中心节点，A，C，E 是其子树。让 $$h_X$$ 表示以X为根节点的子树的高度。通过定义我们知道：

$$
newBal(B)=h_A−h_C
$$

$$
oldBal(B)=h_A−h_D
$$

但我们知道，D 的高度也可以通过 $$1 + max(h_C,h_E)$$ 给定，也就是说，D 的高度为两子树高度中较大者加 1。记住，$$h_C$$ 和 $$h_E$$ 没有改变。所以，把上式代入第二个方程，可以得到：

$$
oldBal(B)=h_A−(1+max(h_C,h_E))
$$

然后两方程作差。下面是作差的步骤，newBal(B) 使用了一些代数方法简化方程。

$$
beginsplitnewBal(B)−oldBal(B)=h_A−h_C−(h_A−(1+max(h_C,h_E)))
$$

$$
newBal(B)−oldBal(B)=h_A−h_C−h_A+(1+max(h_C,h_E))
$$

$$
newBal(B)−oldBal(B)=h_A−h_A+1+max(h_C,h_E)−h_C
$$

$$
newBal(B)−oldBal(B)=1+max(h_C,h_E)−h_C
$$

接下来我们移动 oldBal(B) 到方程的右端并利用 $$max(a,b)−c = max(a−c,b−c)$$。

$$
newBal(B)=oldBal(B)+1+max(h_C−h_C,h_E−h_C)
$$

但 $$h_E − h_C$$ 等同于 −oldBal(D)。所以我们说：max(−a,−b) = −min(a,b)，可以通过以下步骤完成对 newBal(B) 的推导：

$$
newBal(B)=oldBal(B)+1+max(0,−oldBal(D))
$$

$$
newBal(B)=oldBal(B)+1−min(0,oldBal(D))
$$

现在方程所有的项都是已知数。如果我们记得 B 是rotRoot，D 是newRoot，可以看出这正好符合第 16 行的语句：


```
rotRoot.balanceFactor = rotRoot.balanceFactor + 1 - min(0,newRoot.balanceFactor)
```

更新节点 D，以及右旋转后的平衡因子的方程推导与此类似。

现在你可能认为步骤都完全了解了。我们知道如何并且什么时候进行左右旋转，但看看图 6。由于节点 A 的平衡因子是 -2，我们应该做一个左旋转。但是，当我们在左旋转时会发生什么？

![img](/img/in-post/2017-10-21-python-AVL-tree/04.png)

图 6：一棵更难平衡的不平衡树

![img](/img/in-post/2017-10-21-python-AVL-tree/05.png)

图 7：显示的树左旋转后，仍然不平衡。如果我们要做一个右旋转来试图再平衡，又回到了开始的状态。


要解决这个问题，我们必须使用以下规则：

* 如果子树需要左旋转使之平衡，首先检查右节点的平衡因子。如果右节点左重则右节点右旋转，然后原节点左旋转。

* 如果子树需要右旋转使之平衡，首先检查左节点的平衡因子。如果左节点右重则左节点左旋转，然后原节点右旋转。


图 8 显示了这些规则如何解决了我们在图 6 和图 7 中遇到的问题。首先，以 C 为中心右旋转，树变成一个较好的形状；然后，以 A 为中心左旋转，整个子树恢复平衡。


![img](/img/in-post/2017-10-21-python-AVL-tree/06.png)

图 8：右旋转后左旋转

实现这些规则的代码可以从我们“再平衡”（rebalance）的方法中找到，如Listing 3 所示。上面的第一条规则从第二行if语句中实现。第二条规则是由第 8 行elif语句实现。

Listing 3

```
def rebalance(self,node):
  if node.balanceFactor < 0:
         if node.rightChild.balanceFactor > 0:
            self.rotateRight(node.rightChild)
            self.rotateLeft(node)
         else:
            self.rotateLeft(node)
  elif node.balanceFactor > 0:
         if node.leftChild.balanceFactor < 0:
            self.rotateLeft(node.leftChild)
            self.rotateRight(node)
         else:
            self.rotateRight(node)
```

通过保持树的平衡，我们可以确保get方法运行的时间复杂度为 O(log2n)。但问题是put方法的时间复杂度是多少？我们把put操作进行分解。由于每一个新节点都是作为叶节点插入的，每一轮更新所有父节点的平衡因子最多只需要 log2n 次操作，每层执行一次。如果子树是不平衡的最多需要两个旋转把子树恢复平衡。但是，每个旋转的操作的复杂度为 O(1) ，所以即使我们进行put操作最终的复杂度仍然是 O(log2n)。
