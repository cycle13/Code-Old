---
layout:     post
title:      "Pycrypto与RSA密码技术笔记"
subtitle:   "Pycrypto and RSA"
date:       2017-10-19
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

# 密码与通信

密码技术是一门历史悠久的技术。信息传播离不开加密与解密。密码技术的用途主要源于两个方面，加密/解密和签名/验签

在信息传播中，通常有发送者，接受者和窃听者三个角色。假设发送者Master想要写信给接受者Ghost，可是又不想信的内容被别人看到，因此Master需要先对信加密，而Ghost收到信之后又能解密。这样别的人即使窃听盗取了密文也无法解密。其次，如果窃听者并不像破译内容，而是伪造Master发消息给Ghost，那么Master发消息前就得先对机密内容进行签名。

# 密码技术

为了进行加密以及通信，人们发明了很多公开的算法。对称与非对称算法等。常见的加密方式有RSA, AES等算法。对于选择加密算法，一个常识就是使用公开的算法。一方面是这些算法经过实践检验，另一方面对于破译难度和破译条件破译时间都有预估。对于任何加密算法，都是能破解的，不同在于时间上的投入。

# Python密码库–Pycrypto

Python良好的生态，对于加密解密技术都有成熟的第三方库。大名鼎鼎的M2Crypto和Pycrypto，前者非常容易使用，可是安装却非常头疼，不同的系统依赖软件的版本还有影响。后者则比较方面，直接使用pip安装即可。

安装

```
pip install pycrypto
```

# RSA 密码算法与签名

RSA是一种公钥密码算法，RSA的密文是对代码明文的数字的 E 次方求mod N 的结果。也就是将明文和自己做E次乘法，然后再将其结果除以 N 求余数，余数就是密文。RSA是一个简洁的加密算法。E 和 N 的组合就是公钥（public key）。

对于RSA的解密，即密文的数字的 D 次方求mod N 即可，即密文和自己做 D 次乘法，再对结果除以 N 求余数即可得到明文。D 和 N 的组合就是私钥（private key）。

算法的加密和解密还是很简单的，可是公钥和私钥的生成算法却不是随意的。本文在于使用，对生成秘钥对的算法就暂时忽略。使用 Pycrypto生成秘钥对很简单，我们分别为 Master和Ghost各生成一对属于自己的秘钥对。

```
from Crypto import Random
from Crypto.Hash import SHA
from Crypto.Cipher import PKCS1_v1_5 as Cipher_pkcs1_v1_5
from Crypto.Signature import PKCS1_v1_5 as Signature_pkcs1_v1_5
from Crypto.PublicKey import RSA

# 伪随机数生成器
random_generator = Random.new().read
# rsa算法生成实例
rsa = RSA.generate(1024, random_generator)

# master的秘钥对的生成
private_pem = rsa.exportKey()

with open('master-private.pem', 'w') as f:
    f.write(private_pem)

public_pem = rsa.publickey().exportKey()
with open('master-public.pem', 'w') as f:
    f.write(public_pem)

# ghost的秘钥对的生成
private_pem = rsa.exportKey()
with open('master-private.pem', 'w') as f:
    f.write(private_pem)

public_pem = rsa.publickey().exportKey()
with open('master-public.pem', 'w') as f:
    f.write(public_pem)
```

所生成的私钥和公钥大概是这样的：

```
-----BEGIN RSA PRIVATE KEY-----
MIICXQIBAAKBgQDR4Wq9l44lw/thTPyFmSi2hII92EPh90yGXQNL5e7zJPD16j6Q
tr+tIPNSQaVrnmNwrtqyEC2x4Meyp3tdCWPYUF11r2GgDgxKfUByetNG4XqJeUKk
kJ6D6C706mTf/2zsm8KFoNYCYPX1GhvpiTOikHcNlHLCnOD7jbMAovJg/QIDAQAB
AoGBAIz8V6+0NxC3bg4WoSs9j1PL/5F7zV3lucoogSZi9vjuP89x40Vi/a9XCxye
bHi2lSYEz3P92jQ7QuqIBx6gSCi3p2HLjD5LyQeSSMbPe8KSlf52dBUaPthbBceA
IJSBDrE8MKGpulTQKAJ7K3zQUOP2ZZgcKxq2jcQgS6iTENIBAkEA5r7emvwuL0Ob
Maav4o1Ovb5c6OL7bSm1tuLPSKl05WuNYfE6LkqiwOOn5lPvsqhwyI1dJeywVeQz
E+PvcTUR7QJBAOjZ8PxnP5T14fuhbfko4d24Ev+iiTBdq3pMXWvobEFL1ljV6aYE
2JAiMjO/Fzd1WgZhWPa3P+diyTs9mART6VECQQC0LeEXdsn9oDYEbFu1dZBB++8C
75NTJ5m8iJlB7QjZyMUq8Ln0wdUa9+n4ohxvDraa9EADSDJdr4bvBjLH3J/1AkBr
9QfO7kvDU5DXqoujVnoJ4xsj3IbAnt0vEZLKwfLW/0M84si2SU7i3IfsB+/KraT0
ilPF50ZAkEN+LNt7PjBRAkAHBBPME7IbFqxi5Cc/6R12DOMiJbOLDTS12b1J1cwG
p8WMIERsvwWdJw+4NdqjbJcjzeGrXhDBi//JU902TAwy
-----END RSA PRIVATE KEY-----

-----BEGIN PUBLIC KEY-----
MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDR4Wq9l44lw/thTPyFmSi2hII9
2EPh90yGXQNL5e7zJPD16j6Qtr+tIPNSQaVrnmNwrtqyEC2x4Meyp3tdCWPYUF11
r2GgDgxKfUByetNG4XqJeUKkkJ6D6C706mTf/2zsm8KFoNYCYPX1GhvpiTOikHcN
lHLCnOD7jbMAovJg/QIDAQAB
-----END PUBLIC KEY-----
```

# 加密与解密

通常通信的时候，发送者使用接受者的公钥加密，接受者使用接受者私钥解密。

简而言之，Master给Ghost通信，需要加密内容，那么Ghost会生成一个秘钥对，Ghost的公钥ghost-public.pem和私钥ghost-private.pem 。Ghost 把公钥公开给发送者，任何人都可以用来加密，然后Master使用ghost-public.pem进行加密，然后把内容发给Ghost，Ghost再使用ghost-private.pem进行解密。

加密（encrypt）

```
# Master使用Ghost的公钥对内容进行rsa 加密

In [12]: message = 'hello ghost, this is a plian text'
In [13]: with open('ghost-public.pem') as f:
   ....:      key = f.read()
   ....:      rsakey = RSA.importKey(key)
   ....:      cipher = Cipher_pkcs1_v1_5.new(rsakey)
   ....:      cipher_text = base64.b64encode(cipher.encrypt(message))
   ....:      print cipher_text
   ....:
HYQPGB+axWCbPp7PPGNTJEAhVPW0TX5ftvUN2v40ChBLB1pS+PVM3YGT5vfcsvmPZhW8NKVSBp8FwjLUnMn6yXP1O36NaunUzyHwI+cpjlkTwZs3DfCY/32EzeuKuJABin1FHBYUMTOKtHy+eEDOuaJTnZTC7ZBkdha+J88HXSc=
```

解密（decrypt）

```
# Ghost使用自己的私钥对内容进行rsa 解密

In [14]: with open('ghost-private.pem') as f:
   ....:      key = f.read()
   ....:      rsakey = RSA.importKey(key)
   ....:      cipher = Cipher_pkcs1_v1_5.new(rsakey)
   ....:      text = cipher.decrypt(base64.b64decode(encrypt_text), random_generator)
   ....:
In [15]: print text
hello ghost, this is a plian text
In [16]: assert text == message, 'decrypt falied'
```

这样Ghost就能看到Master所发的内容了，当然，如果Ghost想要给Master发消息，就需要Master先把其的公钥给Ghost，后者再使用公钥加密，然后发送给Master，最后Master使用自己的私钥解密。

# 签名与验签

当然，对于窃听者，有时候也可以对伪造Master给Ghost发送内容。为此出现了数字签名。也就是Master给Ghost发送消息的时候，先对消息进行签名，表明自己的身份，并且这个签名无法伪造。具体过程即Master使用自己的私钥对内容签名，然后Ghost使用Master的公钥进行验签。

签名

```
# Master 使用自己的公钥对内容进行签名
In [17]: with open('master-private.pem') as f:
   ....:       key = f.read()
   ....:       rsakey = RSA.importKey(key)
   ....:       signer = Signature_pkcs1_v1_5.new(rsakey)
   ....:       digest = SHA.new()
   ....:       digest.update(message)
   ....:       sign = signer.sign(digest)
   ....:       signature = base64.b64encode(sign)
In [18]: print signature
jVUcAYfgF5Pwlpgrct3IlCX7KezWqNI5tD5OIFTrfCOQgfyCrOkN+/gRLsMiSDOHhFPj2LnfY4Cr5u4eG2IiH8+uSF5z4gUX48AqCQlqiOTLk2EGvyp+w+iYo2Bso1MUi424Ebkx7SnuJwLiPqNzIBLfEZLA3ov69aDArh6hQiw=
```

验签

```
In [22]: with open('master-public.pem') as f:
   ....:      key = f.read()
   ....:      rsakey = RSA.importKey(key)
   ....:      verifier = Signature_pkcs1_v1_5.new(rsakey)
   ....:      digest = SHA.new()
   ....:      # Assumes the data is base64 encoded to begin with
   ....:      digest.update(message)
   ....:      is_verify = signer.verify(digest, base64.b64decode(signature))
   ....:      print is_verify
   ....:
True
```

# 总结

Pycrypto提供了比较完善的加密算法。RSA广泛用于加密与解密，还有数字签名通信领域。使用Publick/Private秘钥算法中，加密主要用对方的公钥，解密用自己的私钥。签名用自己的私钥，验签用对方的公钥。

加密解密：公钥加密，私钥解密

签名验签：私钥签名，公钥验签

无论是加密机密还是签名验签都使用同一对秘钥对
