---
layout:     post
title:      "Github搭建个人主页"
subtitle:   "Build your personal homepage using Github"
date:       2016-12-13
author:     "QQF"
header-img: "img/home-bg.png"
catalog: false
tags:
    - Web开发
    - 我的笔记
---

## 使用 GitHub Pages 建站

- Google 输入 github pages，点击进入。
- 登陆 GitHub，新建一个 repository, 命名为 你的用户名 + github.io。如我的用户名为 QQFRaphael，所以 repository 命名为 qqfraphael.github.io
- `git clone git@github.com:你的用户名/你的用户名.github.io.git`
- 把准备好的模板文件放进项目根目录去。模板文件应当包含下面这些内容：_config.yml文本文件，它是jekyll的设置文件；_layouts目录，用于存放模板文件；进入_layouts目录，创建一个default.html文件，作为默认模板；在项目根目录下创建_posts目录用于存放blog文章；在项目根目录，创建一个index.html文件作为文件首页
- `git add .`
- `git status`
- `git commit -m "first push"`
- `git push`
- 打开浏览器，输入 你的用户名.github.io，进入网站首页
- 可以在[万网](https://www.aliyun.com)购买域名，并绑定到你的网站

## 使用 Jekyll 搭建个人博客

- 安装 Ruby
- `gem install github-pages`安装Jekyll (`gem update github-pages`用于更新)
- 在项目根目录建立一个文件Gemfile，内容如下：

    source 'https://rubygems.org'
    gem 'github-pages'

- `bundle exec jekyll serve`，没有bundle可以用`gem install bundle`安装
- 本地测试在浏览器输入 `http://localhost:4000`即可 
- 调试完成提交github即可

## 以用户为中心的五维设计法则

- 搭建博客的目的
表达自己的想法<br/>
锻炼自己的设计能力<br/>
作为日后找工作的作品<br/>

- 有哪些功能和特性
Post: Easy to read, Easy to search, Tags, Recommended List, Comments<br/>
Design: Modern Style, Blank Space, CSS3, UX, English Version, SEO ,Responsive Design<br/>
About: Self-introduction, Resume, Portfolio, Feedback

- 组合功能和特性
![img](/img/in-post/2016-12-13-Build-your-personal-homepage-using-Github/01.png)

- 界面控件的位置

- 美化界面，视觉设计
