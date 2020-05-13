# Raphael Blog

This is a simple blog website developed by Python.
Falsk, Mongo data base is used in this blog.

This is just a personal blog, which could only:
 - write/modify/delete articles using Markdown
 - count those who read articles
 - list out all the articles

Deployment has already been complished, the IP address is 192.241.217.113, domain name is www.qqfblog.cn.
Nginx, uwsgi is used, CentOs is the OS.
Some setting files are included in this repository.
Deploy it by the following instructions:
 - install the nginx, uwsgi and mongo
 - install all the requirements through pip install -r requirements.txt
 - /etc/selinux/config, change SELINUX=enforcing to SELINUX=disabled
 - service mongod start 
 - setting the uwsgi, and initiate it with: uwsgi --ini /blog/uwsgi.ini -d /blog/log
 - setting the nginx using nginx.conf, then service nginx start
 - Now it is deployed on CentOS.

Future:
 - comments
 - math formula using markdown


In fact I would not continue to maintain this blog because deploy it on VPS costs too much.
