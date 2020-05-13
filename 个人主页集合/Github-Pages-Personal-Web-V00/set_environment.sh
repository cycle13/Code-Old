#!/bin/bash

export PATH="/root/anaconda2/bin:$PATH"
ssserver -c /etc/shadowsocks.json -d start
mongod --dbpath=/root/RaphaelBlog/data/db --logpath=/root/RaphaelBlog/data/mongo.log --fork
nginx
uwsgi --ini /root/RaphaelBlog/uwsgi.ini -d /root/RaphaelBlog/data/uwsgi.log
