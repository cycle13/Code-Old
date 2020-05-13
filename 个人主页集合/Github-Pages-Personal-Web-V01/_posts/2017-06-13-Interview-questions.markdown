---
layout:     post
title:      "那些年面试官问过的题"
subtitle:   "Interview Questions"
date:       2017-06-13
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - 算法
    - 我的笔记
---

> 笔者非计算机科班出身但本科时曾经做过IT类的工作。这里记录一些当时面试时被问到的算法题，并给出当时写出的代码，做一个简单的分享。时间有些久远，可能跟不上时代了。。。

# 二叉树反转

<pre class="prettyprint lang-java linenums">
public class TreeNode(int x) {
    int val;
    TreeNode left;
    TreeNode right;
    TreeNode(int x) {val = x;}
}

public TreeNode invert(TreeNode root) {
    if(root == null) return null;
    
    root.left = invert(root.left);
    root.right = invert(root.right);
    
    TreeNode tmp = root.left;
    root.left = root.right;
    root.right = tmp;
    
    return root;
}
</pre>

# 快速排序

<pre class="prettyprint lang-java linenums">
public void swap(int [] nums, int idx1, int idx2){
    int tmp = nums[idx1];
    nums[idx1] = nums[idx2];
    nums[idx2] = tmp;
}

public int partition(int [] nums, int low, int high) {
    int pivot = nums[high];
    int ii = low;
    for (int jj = low; jj < high; jj++) {
        if (nums[jj] <= pivot) {
            swap(nums, ii++, jj);
        }
    }
    swap(nums, ii, high);
    return ii;
}

public void sort(int [] nums, int low, int high) {
    if (low < high) {
        int ii = partition(nums, low, high);
        sort(nums, low, ii-1);
        sort(nums, ii+1, high);
    }
}
</pre>

# 无环单链表反转

<pre class="prettyprint lang-python linenums">
class ListNode:
    def __init__(self, x):
        self.val = x
        self.next = None

def reverse(head, newhead):
    if head is None:
        return

    if head.next is None:
        newhead = head
    else:
        newhead = reverse(head.next, newhead)
        head.next.next = head
        head.next = None

    return newhead
</pre>