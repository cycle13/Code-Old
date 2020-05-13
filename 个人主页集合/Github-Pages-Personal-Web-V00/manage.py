# -*- coding: utf-8 -*-
#!/usr/bin/env python
import os

from flask_script import Manager, Shell

from pyblog import create_app

config_name = os.getenv('FLASK_CONFIG') or 'default'
app = create_app(config_name)

from views import *

manager = Manager(app)


def make_shell_context():
    return dict(app=app)


manager.add_command("shell", Shell(make_context=make_shell_context))


@manager.option('-u', '--userName', dest='userName', default=app.config['DEFAULT_USER'])
@manager.option('-p', '--password', dest='password', default='123456')
def create_user(userName, password):
    mongo.db.users.remove({"userName": userName})
    mongo.db.users.insert({'userName': userName, 'password': generate_password_hash(password)})


if __name__ == '__main__':
    manager.run()
