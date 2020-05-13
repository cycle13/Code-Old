#!/bin/sh
# 清理后再生成Jekyll网页
bundle exec jekyll clean
bundle exec jekyll build
# 在网页目录建立git
cd _site
git init
# 添加VPS上的裸仓库作为远程仓库
git remote add jekyll root@107.174.206.90:/root/jekyll
# 提交并推送，这里的commit信息我定的是提交时间
git add .
commit_time=$(date "+%Y-%m-%d %H:%M:%S")
git commit -m "${commit_time}"
git push -f jekyll master:master
# 删除本地网页文件夹
cd ../
rm -rf _site