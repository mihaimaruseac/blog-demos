#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys

MIN_NEXT_DELTA = 1 * 60
MIN_LAST_DELTA = 5 * 60
MAX_CONV_LEN = 15 * 60

class Line:
    def __init__(self, tstamp, user, reply):
        self.tstamp = tstamp
        self.user = user
        self.reply = reply

    def __str__(self):
        return "%s %s: %s" % (self.tstamp, self.user, self.reply)

class Conversation:
    def __init__(self, tstamp, user, reply):
        self.lines = [Line(tstamp, user, reply)]
        self.users = set(user)
        self.tstamp = Tstamp(tstamp)
        self.user = user
        self.start = Tstamp(tstamp)

    def add(self, tstamp, user, reply):
        self.lines.append(Line(tstamp, user, reply))
        self.users.add(user)
        self.user = user
        self.tstamp = Tstamp(tstamp)
        if not self.start: self.start = self.tstamp

    def __str__(self):
        strr = '<%s - %s (%s)>\n' % (self.start, self.tstamp,
                self.tstamp - self.start)
        for line in self.lines:
            strr += str(line) + '\n'
        strr += '\n'
        return strr

    def __repr__(self):
        return self.__str__()

class Tstamp:
    def __init__(self, s):
        [self.h, self.m, self.s] = map(int, s.split(':'))

    def __sub__(self, other):
        a = self.h * 3600 + self.m * 60 + self.s
        b = other.h * 3600 + other.m * 60 + other.s
        return a - b

    def __str__(self):
        return '%s:%s:%s' % (self.h, self.m, self.s)

    def __repr__(self):
        return self.__str__()

usersNow = {}
conversations = []
allConversations = []

def make_user_active(user):
    usersNow[user] = True

def make_user_inactive(user):
    usersNow[user] = False

def makeConversation(tstamp, user, text):
    global conversations
    c = Conversation(tstamp, user, text)
    conversations.append(c)

def get_closest_conversation(now, proximity):
    if not conversations:
        return None
    c = min(conversations, key = lambda c: now - c.tstamp)
    if (now - c.tstamp < MIN_NEXT_DELTA or not proximity) or \
        now - c.start < MAX_CONV_LEN:
        return c
    return None

def get_nick_ref(now, text):
    for c in conversations:
        if now - c.start > MAX_CONV_LEN: continue
        #print text
        candidates = [text[0][:-1]] + text
        #print candidates

def log_reply(tstamp, user, text):
    now = Tstamp(tstamp)
    conv = get_nick_ref(now, text)
    conv = conv or get_closest_conversation(now, True)
    conv = conv or get_closest_conversation(now, False)

    if conv:
        conv.add(tstamp, user, ' '.join(text))
    else:
        makeConversation(tstamp, user, ' '.join(text))

def parse_line(tstamp, user, text):
    if user == '---':
        action = text[0][:-1]

        if action == 'join':
            make_user_active(text[1])
            return
        elif action in ['part', 'quit', 'kick']:
            make_user_inactive(text[1])
            return
        elif action in ['log', 'mode', 'topic', 'names']:
            return
        elif action == 'nick':
            # print text[1], text[3] # record nick changes
            return
        print user, text, text[0][:-1]
    elif user == '*' or not text:
        return # skip emotions, they don't give insights
    else:
        log_reply(tstamp, user[1:-1], text)

def reset():
    global allConversations, conversations
    allConversations += conversations
    conversations = []

def main():
    global allConversations, users, conversations
    for fname in sys.argv[1:]:
        reset()
        with open(fname, "r") as f:
            while True:
                line = f.readline()
                if not line: break
                line = line[:-1]

                parts = line.split()
                tstamp = parts[0]
                user = parts[1]
                parse_line(tstamp, user, parts[2:])
    reset()
    print allConversations

if __name__ == '__main__':
    main()
