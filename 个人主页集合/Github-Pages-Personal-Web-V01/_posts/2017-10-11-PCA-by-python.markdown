---
layout:     post
title:      "Python实现PCA降维"
subtitle:   "PCA by Python"
date:       2017-10-11
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
    - 机器学习
---

# 特征降维之PCA

特征降维是无监督学习的另一个应用，在图像识别方面，经常会遇到特征维度非常高的训练样本(像素矩阵)时，很难提供数据展现，训练学习模型也很耗时耗力，特征降维不仅重构了有效的低维度特征向量，也为数据展现提供了可能，其中主成分分析(Principal Component Analysis)是应用最广泛的特征降维技术。

PCA简单来说，是一种特征选择(重构)手段，将原来的特征空间做了映射，使得新的映射后特征空间数据彼此正交，尽可能保留下具备区分性的低维度数据特征。特征不需要标准化处理！！，因为是重构。 下面以手写体数字(sklearn库内置)识别为例：

# 获取训练，测试样本

```
# 导入数据加载器
>>> from sklearn.datasets import load_digits
>>> digits = load_digits()
# 64维，1797条数据
>>> digits.data.shape
(1797, 64)
>>> digits.target.shape
(1797,)
# 切割数据，25%测试
>>> from sklearn.cross_validation import train_test_split
X_train, X_test, y_train, y_test = train_test_split(digits.data, digits.target, test_size=0.25, random_state=33)
```

# 直接SVM模型性能评估

```
>>> from sklearn.preprocessing import StandardScaler
>>> from sklearn.svm import LinearSVC
# 标准化数据
>>> ss = StandardScaler()
>>> X_dire_train = ss.fit_transform(X_train)
>>> X_dire_train.shape
(1347, 64)
>>> X_dire_test = ss.transform(X_test)
# 线性核函数初始化
>>> lsvc = LinearSVC()
# 训练模型
>>> lsvc.fit(X_dire_train, y_train)
# 预测
>>> y_dire_predict = lsvc.predict(X_dire_test)
```

# 直接SVM模型性能评估

```
# 使用模型自带的评估函数进行准确性测评 
>>> print('The Accuracy of Linear SVC is', lsvc.score(X_dire_test, y_test))
# 导入classification_report模块对预测结果做更加详细的分析
from sklearn.metrics import classification_report
print(classification_report(y_test, y_dire_predict, target_names=digits.target_names.astype(str)))
```

显示

```
The Accuracy of Linear SVC is 0.953333333333
 		precision    recall  f1-score   support
  0       0.92      1.00      0.96        35
  1       0.96      0.98      0.97        54
  2       0.98      1.00      0.99        44
  3       0.93      0.93      0.93        46
  4       0.97      1.00      0.99        35
  5       0.94      0.94      0.94        48
  6       0.96      0.98      0.97        51
  7       0.92      1.00      0.96        35
  8       0.98      0.84      0.91        58
  9       0.95      0.91      0.93        44

avg/total  0.95     0.95      0.95        450
```

# PCA特征降维

```
>>> from sklearn.decomposition import PCA
>>> estimator = PCA(n_components=20)   # 初始化，64维压缩至20维
# 利用训练特征决定（fit）20个正交维度的方向，并转化（transform）原训练特征
>>> pca_X_train = estimator.fit_transform(X_train)
>>> pca_X_train.shape
(1347, 20)   # 维度从64变为20
# 测试特征也按照上述的20个正交维度方向进行转化（transform）
>>> pca_X_test = estimator.transform(X_test)
```

# 线性核函数的支持向量机分类

```
from sklearn.svm import LinearSVC
pca_svc = LinearSVC()
pca_svc.fit(pca_X_train, y_train)  # 训练模型 
y_predict = pca_svc.predict(pca_X_test) # 进行预测
```

# 模型性能评价

```
>>> from sklearn.metrics import classification_report
# 自带评价函数
>>> print('The Accuracy of Linear SVC after PCA is', pca_svc.score(pca_X_test,y_test))
# 详细评价精确率，回调率，f1指数
>>> print(classification_report(y_test, y_predict, target_names=np.arange(10).astype(str)))
```

显示：
 
```
The Accuracy of Linear SVC after PCA is 0.935555555556
		precision    recall  f1-score   support
  0       0.94      0.97      0.96        35
  1       0.88      0.93      0.90        54
  2       1.00      0.98      0.99        44
  3       0.91      0.89      0.90        46
  4       1.00      0.94      0.97        35
  5       0.94      0.92      0.93        48
  6       0.96      0.98      0.97        51
  7       0.95      1.00      0.97        35
  8       0.90      0.90      0.90        58
  9       0.93      0.89      0.91        44

avg/total  0.94     0.94      0.94        450
```

# 总结

相比于将样本数据不降维处理直接拿来训练，PCA降维处理后数据的会损失一点预测准确性(约0.02)，因为在降维过程中，尽管规避掉了大量的特征冗余和噪声，但是也会损失一些有用的模式信息，但是维度的大大压缩不仅节省了大量模型训练时间，也降低了模型的训练难度，对于高维样本来说是划算的选择。