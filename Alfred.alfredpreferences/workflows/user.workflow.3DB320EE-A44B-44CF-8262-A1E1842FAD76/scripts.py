# encoding: utf-8
# -*- coding: utf-8 -*-

from __future__ import print_function, unicode_literals

import sys
import os
import subprocess
import time

from workflow import Workflow, ICON_WARNING, ICON_INFO, ICON_ERROR


def parseTime(time):
    if time.isdigit():
        return int(time)

    table = {
        's': 1,
        'sec': 1,
        'secs': 1,
        'second': 1,
        'seconds': 1,
        'm': 60,
        'min': 60,
        'mins': 60,
        'minute': 60,
        'minutes': 60,
        'h': 60*60,
        'hour': 60*60,
        'hours': 60*60
    }
 
    for expr in table:
        if time.endswith(expr):
            firstPart = time[:-(len(expr))]
            if firstPart.isdigit():
                return int(firstPart) * table[expr]
 
    return 3*60

# def showNotification(wf, message):
#     command = "/usr/bin/osascript -e 'display notification \"%s\" sound name \"\"'" % (message)
#     os.system(command)

def playAlertSound():
    os.system("afplay /System/Library/PrivateFrameworks/ScreenReader.framework/Versions/A/Resources/Sounds/EnterInvisibleArea.aiff")

class ScriptRunner(object):

    def __init__(self):
        self.wf = None

    def run(self, wf):
        self.wf = wf

        timeStr = "3"
        message = "Time's up!"

        if wf.args:
            timeStr = wf.args[0].strip()
        if len(wf.args) > 1:
            self.wf.logger.debug("wf.args")
            self.wf.logger.debug(wf.args)
            message = " ".join(wf.args[1:])

        time.sleep(parseTime(timeStr))
        print(message.encode('utf8'))
        playAlertSound()

if __name__ == '__main__':
    wf = Workflow()
    app = ScriptRunner()
    wf.run(app.run)