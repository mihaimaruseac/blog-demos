#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import socket
import string
import os

HOST = 'chat.freenode.net'
PORT = 6667
NICK = 'irCri'
IDENT = 'MM'
REALNAME = 'Mihai Maruseac'
OWNER = 'Mihai Maruseac'
CHANNELINIT = '#mm_test'
readbuffer = ''

s=socket.socket()
s.connect((HOST, PORT))
s.send('NICK ' + NICK + 'n')
s.send('USER ' + IDENT + ' ' + HOST + ' bla :' + REALNAME + 'n')

def syscmd(commandline,channel):
    cmd=commandline.replace('sys ','')
    cmd=cmd.rstrip()
    os.system(cmd+' >temp.txt')
    a=open('temp.txt')
    ot=a.read()
    ot.replace('n','|')
    a.close()
    s.send('PRIVMSG '+channel+' :'+ot+'n')

def parsemsg(msg):
    complete = msg[1:].split(':',1)
    info = complete[0].split(' ')
    msgpart = complete[1]
    sender = info[0].split('!')
    if msgpart[0] == '!' and sender[0] == OWNER:
        cmd = msgpart[1:].split(' ')
        if cmd[0] == 'op':
            s.send('MODE ' + info[2] + ' +o ' + cmd[1] + 'n')
        if cmd[0] == 'deop':
            s.send('MODE ' + info[2] + ' -o ' + cmd[1]+'n')
        if cmd[0] == 'voice':
            s.send('MODE ' + info[2] + ' +v ' + cmd[1] + 'n')
        if cmd[0] == 'devoice':
            s.send('MODE ' + info[2] + ' -v ' + cmd[1] + 'n')
        if cmd[0] == 'sys':
            syscmd(msgpart[1:],info[2])

    if msgpart[0] == '-' and sender[0] == OWNER:
        cmd = msgpart[1:]
        s.send(cmd + 'n')
        print 'cmd=' + cmd

while 1:
    line=s.recv(500)
    if not line:
        continue
    print line
    if line.find('No Ident response')!=-1:
        print "JOINING..."
        s.send('JOIN ' + CHANNELINIT + '\n')
    if line.find('PRIVMSG')!=-1:
        parsemsg(line)
        line = line.rstrip()
        line = line.split()
        if(line[0] == 'PING'):
            s.send('PONG ' + line[1] + '\n')

