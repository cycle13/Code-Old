# -*- coding: utf-8 -*-
import datetime
import re

from flask_wtf import Form
from wtforms import StringField, PasswordField, BooleanField, SubmitField, TextAreaField, HiddenField
from wtforms.validators import DataRequired, Length


class LoginForm(Form):
    userName = StringField(u'用户名', validators=[DataRequired(), Length(1, 64)])
    password = PasswordField(u'密码', validators=[DataRequired()])
    rememberMe = BooleanField(u'记住我？')
    submit = SubmitField(u'登录')


class PostForm(Form):
    title = StringField(u'标题', validators=[DataRequired(), Length(1, 64)])
    markdown = TextAreaField(u'正文', validators=[DataRequired()])
    html = HiddenField()
    submit = SubmitField(u'发布')

    def getBlog(self):
        return {'title': self.title.data, 'markdown': self.markdown.data, 'html': self.html.data,
                'postTime': datetime.datetime.now(), 'summary': self.generateSummary(self.html.data)}

    def generateSummary(self, html):
        summaryLen = 200
        if (len(html) < summaryLen):
            summaryLen = len(html)
        return self.trimHtmlTag(html)[0:summaryLen - 1]

    def trimHtmlTag(self, html):
        regex = re.compile(r'<[^>]+>', re.S)
        return regex.sub('', html)
