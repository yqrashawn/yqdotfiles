#!/usr/bin/env python
#-*- coding:UTF-8 -*-
#
# @author  Ritashugisha
# @contact ritashugisha@gmail.com
# @program Luxinate - Alfred.v2 Streamed Media Downloader
# @version <v$VERSIONr$RELEASE>
#
# Luxinate - Alfred.v2 Streamed Media Downloader
# Copyright (C) 2014 Ritashugisha
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# [Imports]
import os
import re
import sys
import json
import types
import datetime
import time
import pickle
import sqlite3
import tempfile
import getpass
import subprocess
import inspect
import logging
import webbrowser
import urlparse
import urllib
import urllib2
import xml.etree.ElementTree as etree
import xml.dom.minidom as dom

# [Global Variables]
AUTHOR      = 'Ritashugisha'
CONTACT     = 'ritashugisha@gmail.com'
PROGRAM     = 'Luxinate'
DESCRIPTION = 'Alfred.v2 Streamed Media Downloader'
VERSION     = '7.01'
RELEASE     = '1.2a'
DISCLAIMER  = '%s %s - %s' % (datetime.datetime.now().strftime('%Y'), AUTHOR, PROGRAM)
ABOUT       = '_about.md'

# [Program Variables]
TMP 	= '/tmp/Luxinate/'
TMP_LOG = '%slogs/' % TMP
TMP_OBJ = '%sobjs/' % TMP
LOG 	= '%slux_%s.log' % (TMP_LOG, time.strftime('%Y-%m-%d'))
PICKLE  = '%sluxTransferObject.pickle' % TMP_OBJ

# [Logging Setup]
for i in [TMP, TMP_LOG, TMP_OBJ, LOG, PICKLE]:
    if not os.path.exists(i):
        if i[-1] != '/':
            os.system('touch %s' % i)
        else:
            os.system('mkdir %s' % i)
logging.basicConfig(level = logging.DEBUG,
    format = '[%(asctime)s] [%(levelname)-8s] <%(funcName)s, %(lineno)d>....%(message)s',
    datefmt = '%Y-%m-%d %H:%M:%S',
    filename = LOG)
console = logging.StreamHandler()
console.setLevel(logging.DEBUG)
console.setFormatter(logging.Formatter('[%(levelname)-8s] <%(funcName)s, %(lineno)d>....%(message)s'))
logging.getLogger('').addHandler(console)
# reload and .setdefaultencoding important for valid XML filesave with strange unicode characters
reload(sys)
sys.setdefaultencoding('utf-8')

"""
.. py:function Logger()
Generate a logging object during runtime.
"""
def Logger(function):
    return logging.getLogger(function)


"""
.. py:class Feedback()
Used to build an XML formatted feedback for Alfred's script filter function.
"""
class Feedback():

    """
    .. py:fucntion __init__(self)
    Feedback element for script filter feedback.
    """
    def __init__(self):
        self.log = Logger('feedback')
        self.feedback = etree.Element('items')
        self.log.info('built feedback object at (%s)' % self.feedback)

    """
    .. py:function __repr__(self)
    Return the parsed script filter feedback as string.
    """
    def __repr__(self):
        result = etree.tostring(self.feedback)
        self.log.info('retrieving feedback object (%s)' % result)
        return result

    """
    .. py:function addItem(self, title, subtitle='', arg='', valid='yes', autocompltet='', icon='icon.png')
    Add an item for script filter feedback.

    :param str title: Title of item
    :param str subtitle: Subtitle of item
    :param str arg: Passed argument of item
    :param str valid: "yes" if valid item, "no" if invalid item
    :param str autocomplete: "yes" if item autocompletes
    :param str icon: Icon file location of item
    """
    def addItem(self, title, subtitle = '', arg = '', valid = 'yes', autocomplete = '', icon = 'icon.png'):
        self.log.info('adding item to feedback {"title":"%s", "subtitle":"%s", "arg":"%s", "icon":"%s"}' % (title, subtitle, arg, icon))
        item = etree.SubElement(self.feedback, 'item', uid = str(len(self.feedback)), arg = arg, valid = valid, autocomplete = autocomplete)
        itemTitle = etree.SubElement(item, 'title')
        itemTitle.text = title
        itemSubtitle = etree.SubElement(item, 'subtitle')
        itemSubtitle.text = subtitle
        itemIcon = etree.SubElement(item, 'icon')
        itemIcon.text = icon


"""
.. py:class Config()
Manage and edit Luxinate's configuration.
"""
class Config():

    """
    .. py:function __init__(self, config)
    Open or write a new configuration file for Luxinate.

    :param str config: File location of configuration file
    """
    def __init__(self, config):
        self.log = Logger('config')
        self.utils = Utils()
        self.config = config
        if not os.path.exists(self.config) or len(open(self.config, 'r').read()) <= 0:
            self.log.info('config does not exist at (%s)' % self.config)
            os.system('touch %s' % self.utils.formatConsole(self.config))
            self.buildTemplate()

    """
    .. py:function write(self, xmlRoot)
    Write the ElementTree root to configuration file.

    :param ElementTree xmlRoot: Root of configuration XML
    """
    def write(self, xmlRoot):
        bump = open(self.config, 'w+')
        bump.write(dom.parseString(etree.tostring(xmlRoot, 'utf-8')).toxml())
        bump.close()

    """
    .. py:function getRoot(self)
    Return the ElementTree root of the configuration file.
    """
    def getRoot(self):
        return etree.ElementTree(file = self.config).getroot()

    """
    .. py:function buildTemplate(self)
    Build the basetemplate of Luxinate's XML configuration file.
    """
    def buildTemplate(self):
        self.log.info('....building config at (%s)' % self.config)
        root = etree.Element('root')
        info = etree.SubElement(root, 'info')
        author = etree.SubElement(info, 'author')
        author.set('contact', CONTACT)
        author.text = AUTHOR
        version = etree.SubElement(info, 'version')
        version.set('release', RELEASE)
        version.text = VERSION
        program = etree.SubElement(info, 'program')
        program.set('description', DESCRIPTION)
        program.text = PROGRAM
        python = etree.SubElement(info, 'python')
        python.set('required', '2.7')
        python.text = str('%s.%s' % (sys.version_info[0], sys.version_info[1]))
        settings = etree.SubElement(root, 'settings')
        download_dir = etree.SubElement(settings, 'download_dir')
        download_dir.set('default', '%s/Downloads/' % os.path.expanduser('~'))
        download_dir.text = download_dir.attrib['default']
        video_opt = etree.SubElement(settings, 'video_opt')
        video_opt.set('default', '-1')
        video_opt.text = video_opt.attrib['default']
        audio_opt = etree.SubElement(settings, 'audio_opt')
        audio_opt.set('default', '.mp3')
        audio_opt.text = audio_opt.attrib['default']
        progress_bar = etree.SubElement(settings, 'progress_bar')
        progress_bar.set('default', 'False')
        progress_bar.text = progress_bar.attrib['default']
        multi_pid = etree.SubElement(settings, 'multi_pid')
        multi_pid.set('default', '-1')
        multi_pid.text = multi_pid.attrib['default']
        history = etree.SubElement(root, 'history')
        history.set('results', '0')
        about = etree.SubElement(root, 'about')
        about.text = 'False'
        self.write(root)
        self.log.info('....config built at (%s)' % self.config)

    """
    .. py:function getAbout(self)
    Retrieve the about from the configuration file.
    """
    def getAbout(self):
        aboutValue = 't' in self.getRoot().find('.//about').text.lower()
        self.log.info('retrieving config about (%s)' % str(aboutValue))
        return aboutValue

    """
    .. py:function toggleAbout(self)
    Toggle the about value between False and True.
    """
    def toggleAbout(self):
        root = self.getRoot()
        root.find('.//about').text = str(not self.getAbout())
        self.log.info('toggling about to (%s)' % str(not self.getAbout()))
        self.write(root)

    """
    .. py:function addHistoryEntry(self, title, url)
    Add a new history entry to the configuration file.

    :param str title: Title of media
    :param str url: URL of media
    """
    def addHistoryEntry(self, title, url):
        self.log.info('adding history entry {"title":"%s", "url":"%s"}' % (title, url))
        root = self.getRoot()
        history = root.find('.//history')
        entry = etree.SubElement(history, 'entry')
        entry.set('title', title.decode('utf-8'))
        entry.set('time', str(time.time()))
        entry.text = url
        history.set('results', str(int(history.attrib['results']) + 1))
        self.write(root)

    """
    .. py:function getHistory(self)
    Return the history entries as a list of dictionaries.
    """
    def getHistory(self):
        history = self.getRoot().find('.//history')
        self.log.info('retrieving [%s] results from history' % history.attrib['results'])
        result = []
        for i in history.findall('entry'):
            entry = {}
            entry['title'] = i.attrib['title']
            entry['time'] = i.attrib['time']
            entry['url'] = i.text
            result.append(entry)
        return result

    """
    .. py:function clearHistory(self)
    Clear all history entries from the configuration file.
    """
    def clearHistory(self):
        root = self.getRoot()
        history = root.find('.//history')
        self.log.info('deleting [%s] results from history' % history.attrib['results'])
        for i in history.findall('entry'):
            self.log.info('....removing {"title":"%s", "url":"%s"} from history' % (i.attrib['title'], i.text))
            history.remove(i)
        history.set('results', '0')
        self.write(root)

    """
    .. py:function toggleProgressBar(self)
    Toggle the progress bar setting from true to false.
    """
    def toggleProgressBar(self):
        root = self.getRoot()
        progress_bar = root.find('.//progress_bar')
        if 'f' in progress_bar.text[0].lower():
            progress_bar.text = 'True'
        else:
            progress_bar.text = 'False'
        self.log.info('toggled progress_bar to (%s)' % progress_bar.text)
        self.write(root)

    """
    .. py:function getProgressBar(self)
    Return the current boolean value of the progress bar.
    """
    def getProgressBar(self):
        progress_bar = 't' in self.getRoot().find('.//progress_bar').text[0].lower()
        self.log.info('retrieving value of progress_bar (%s)' % progress_bar)
        return progress_bar

    """
    .. py:function getMultiPid(self)
    Return the current value of the multi pid.
    """
    def getMultiPid(self):
        multi_pid = self.getRoot().find('.//multi_pid').text
        self.log.info('retrieving value of multi_pid (%s)' % multi_pid)
        return multi_pid

    """
    .. py:function editMultiPid(self, pid, default=False)
    Change the value of the multi pid in the configuration.

    :param str pid: New PID
    :param bool defualt: True if revert to default value, False otherwise
    """
    def editMultiPid(self, pid, default = False):
        root = self.getRoot()
        multi_pid = root.find('.//multi_pid')
        if not default:
            multi_pid.text = str(pid)
        else:
            multi_pid.text = multi_pid.attrib['default']
        self.log.info('changed multi_pid to (%s)' % multi_pid.text)
        self.write(root)

    """
    .. py:function editDownloadDir(self, directory, default=False)
    Change the value of the download directory in the configuration.

    :param str directory: New directory location
    :param bool default: True if revert to default value, False otherwise
    """
    def editDownloadDir(self, directory, default = False):
        root = self.getRoot()
        download_dir = root.find('.//download_dir')
        if not default:
            if directory[:-1] != '/':
                directory = '%s/' % directory
            download_dir.text = directory
        else:
            download_dir.text = download_dir.attrib['default']
        self.log.info('changed download_dir to (%s)' % download_dir.text)
        self.write(root)

    """
    .. py:function getDownloadDir(self)
    Return the current download location.
    """
    def getDownloadDir(self):
        download_dir = self.getRoot().find('.//download_dir').text
        self.log.info('retrieving value of download_dir (%s)' % download_dir)
        return download_dir

    """
    .. py:function editVideoOpt(self, opt, default=False)
    Change the value of the video option in the configuration.

    :param str opt: New option for video
    :param bool default: True if revert to default value, False otherwise
    """
    def editVideoOpt(self, opt, default = False):
        root = self.getRoot()
        video_opt = root.find('.//video_opt')
        if not default:
            video_opt.text = opt
        else:
            video_opt.text = video_opt.attrib['default']
        self.log.info('changed video_opt to (%s)' % video_opt.text)
        self.write(root)

    """
    .. py:function getVideoOpt(self)
    Get the current video option.
    """
    def getVideoOpt(self):
        video_opt = self.getRoot().find('.//video_opt').text
        self.log.info('retireving value of video_opt (%s)' % video_opt)
        return video_opt

    """
    .. py:function editAudioOpt(self, opt, default=False)
    Change the value of the audio option in the configuration.

    :param str opt: New option for audio
    :param bool default: True if revert to default value, False otherwise
    """
    def editAudioOpt(self, opt, default = False):
        root = self.getRoot()
        audio_opt = root.find('.//audio_opt')
        if not default:
            audio_opt.text = opt
        else:
            audio_opt.text = audio_opt.attrib['default']
        self.log.info('changed audio_opt to (%s)' % audio_opt.text)
        self.write(root)

    """
    .. py:function getAudioOpt(self)
    Get the current audio option.
    """
    def getAudioOpt(self):
        audio_opt = self.getRoot().find('.//audio_opt').text
        self.log.info('retrieving value of audio_opt (%s)' % audio_opt)
        return audio_opt


"""
.. py:class Utils()
Used for preforming system operations.
"""
class Utils():

    """
    .. py:function __init__(self)
    Initialize the Utils object.
    """
    def __init__(self):
        self.log = Logger('utils')
        self.typeVideo = ['.flv', '.mp4', '.mov', '.avi', '.mpeg', '.wmv']
        self.typeAudio = ['.mp3', '.wav', '.m4a', '.ogg', '.flac', '.wma', '.mp2', '.acc', '.aiff']

    """
    .. py:function runProcess(self, process)
    Run a subprocess on the system.

    :param str process: Process to be run
    """
    def runProcess(self, process):
        self.log.info('running subprocess (%s)' % process)
        proc = subprocess.Popen([process], stdout = subprocess.PIPE, shell = True)
        (proc, proc_e) = proc.communicate()
        return proc

    """
    .. py:function runOsascript(self, script)
    Run an osascript on the system via subprocess.

    :param str script: Osascript to be run
    """
    def runOsascript(self, script):
        self.log.info('running osascript (%s)' % script)
        proc = self.runProcess('osascript -e \'%s\'' % script)
        return proc

    """
    .. py:function formatConsole(self, string)
    Escape unfriendly characters from a string.

    :param str string: String to be escaped
    """
    def formatConsole(self, string):
        bannedChars = ['!', '?', '$', '%', '#', '&', '*', ';', '(', ')', '@', '`', '|', "'", '"', '~', '<', '>', ' ']
        for i in bannedChars:
            if i in string:
                string = string.replace(i, '\%s' % i)
        return string

    """
    .. py:function replaceExtension(self, filename, extension)
    Replace and return a filename's extension.

    :param str filename: Filename to change
    :param str extension: New extension of filename
    """
    def replaceExtension(self, filename, extension):
        if extension[0] != '.':
            extension = '.%s' % extension
        return '%s%s' % (os.path.splitext(filename)[0], extension)

    """
    .. py:function getMediaType(self, mediafile)
    Check if the mediafile given is of typeAudio or typeVideo.

    :param str mediafile: Filename of media
    """
    def getMediaType(self, mediafile):
        (fileName, fileExtension) = os.path.splitext(mediafile)
        if fileExtension.lower() in self.typeVideo:
            return 1
        elif fileExtension.lower() in self.typeAudio:
            return 2
        else:
            return 0

    """
    .. py:function isInt(self, string)
    Check in a passed string is recognized as an integer.

    :param str string: String of possible integer
    """
    def isInt(self, string):
        try:
            int(string)
            return True
        except:
            return False

    """
    .. py:function getFirefoxLast(self)
    Attempt to get the last loaded URL from Firefox.
    """
    def getFirefoxLast(self):
        mozdatb = '%s/Library/Application Support/Firefox/Profiles/' % os.path.expanduser('~')
        mozcurr = sqlite3.connect(['%s%s/places.sqlite' % (mozdatb,i) for i in os.listdir(mozdatb) if '.default' in i[(len(i)-8):].lower()][0]).cursor()
        return mozcurr.execute("SELECT datetime(moz_historyvisits.visit_date/1000000, 'unixepoch'), moz_places.url FROM moz_places, moz_historyvisits WHERE moz_places.id = moz_historyvisits.place_id").fetchall()[-1][1]


"""
.. py:class PickleTransfer()
Used to transfer object data from Luxinate to Download in pickle form.
"""
class PickleTransfer():

    """
    .. py:function __init__(self, mediatitle, mediafile, mediaurl)
    Build an object used for pickle transfer.

    :param str mediatitle: Title of the media
    :param str mediafile: Filename of the media
    :param str mediaurl: URL of the media
    """
    def __init__(self, mediaurl, mediaInfo):
        self.mediaurl = mediaurl
        self.mediaInfo = mediaInfo
        self.mediaFormats = None


"""
.. py:class binaries()
Used for pretty much everything.
"""
class Binaries():

    """
    .. py:function __init__(self)
    Initialize a new set of binaries for any calling class.
    """
    def __init__(self):
        self.log          = Logger('binaries')
        self.log.info('initialized new set of binaries at (%s)' % self)
        self.workflow     = '%s/' % os.path.dirname(os.path.abspath(__file__))
        self.config       = Config('%s_config.xml' % self.workflow)
        self.resources    = '%sResources/' % self.workflow
        self.icons        = '%sIcons/' % self.resources
        self.glyphmgr     = '%sGlyphManager' % self.workflow
        self.qlmdmgr      = '%sQLMarkdown.qlgenerator' % self.resources
        self.pkgmanager   = '%sLuxePrisimPackageManager' % self.workflow
        self.updater      = '%sLuxinateUpdater' % self.workflow
        self.youtube_dl   = '%syoutube-dl' % self.resources
        self.ffmpeg       = '%sffmpeg' % self.resources
        self.sender       = 'com.runningwithcrayons.Alfred-2'
        self.cocoa        = Cocoa('/Applications/cocoaDialog.app/Contents/MacOS/cocoadialog')
        self.notifier     = Notifier('/Applications/terminal-notifier.app/Contents/MacOS/terminal-notifier')
        self.dependencies = [
            {'title':'mstratman.cocoadialog', 'dest':'/Applications', 'loci':self.cocoa.cocoa},
            {'title':'alloy.terminal-notifier', 'dest':'/Applications', 'loci':self.notifier.notifier},
            {'title':'ritashugisha.glyphmanager', 'dest':self.workflow, 'loci':self.glyphmgr},
            {'title':'manolin.inkmark', 'dest':self.resources, 'loci':self.qlmdmgr},
            {'title':'rg3.youtube-dl', 'dest':self.resources, 'loci':self.youtube_dl},
            {'title':'tessus.ffmpeg', 'dest':self.resources, 'loci':self.ffmpeg},
            {'title':'ritashugisha.luxinateicons', 'dest':self.resources, 'loci':self.icons},
            {'title':'ritashugisha.luxinateupdater', 'dest':self.workflow, 'loci':self.updater}]
        self.typeVideo    = ['.flv', '.mp4', '.mov', '.avi', '.mpeg', '.wmv']
        self.typeAudio    = ['.mp3', '.wav', '.m4a', '.ogg', '.flac', '.wma', '.mp2', '.acc', '.aiff']

"""
.. py:class Updates()
Used to update resources such as YouTube-DL.
"""
class Updates():

    """
    .. py:function __init__(self)
    Initialize the updates object.
    """
    def __init__(self):
        self.log = Logger('updates')
        self.log.info('initialized updates object')
        self.binaries = Binaries()
        self.utils = Utils()
        self.lux_update = 'https://raw.githubusercontent.com/Ritashugisha/Luxinate/master/Versions/versions.json'
        self.rg3_update = 'http://rg3.github.io/youtube-dl/update/versions.json'
        StartUp().startUp()

    """
    .. py:function updates(self)
    Luxinate to Alfred interaction for Updates.
    """
    def updates(self):
        feed = Feedback()
        feed.addItem(PROGRAM, 'Check for any updates for %s...' % PROGRAM, '1', '', '', '%s_update.png' % self.binaries.icons)
        feed.addItem('YouTube-DL', 'Check for any updates for youtube-dl...', '2', '', '', '%s_update.png' % self.binaries.icons)
        return feed

    """
    .. py:function updatesDetermine(self, query)
    Determine which function to run.

    :param str query: Query used to choose update to run
    """
    def updatesDetermine(self, query):
        query = int(query)
        if query == 1:
            self.updateSelf()
        else:
            self.updateYouTubeDL()

    """
    ..py:function updateSelf(self)
    Run process necessary to update self.
    """
    def updateSelf(self):
        self.utils.runProcess('%s -l "%sLuxinate.py"' % (self.utils.formatConsole(self.binaries.updater), self.binaries.workflow))

    """
    .. py:function updateYouTubeDL(self)
    Update to the latest version of YouTube-DL.
    """
    def updateYouTubeDL(self):
        rg3 = json.loads(urllib.urlopen(self.rg3_update).read())
        latest = rg3['latest']
        local = self.utils.runProcess('%s --version' % self.utils.formatConsole(self.binaries.youtube_dl))
        latestVersion = '.'.join(latest.split('.', 2)[:2])
        latestRelease = latest.split('.')[-1]
        localVersion = '.'.join(local.split('.', 2)[:2])
        localRelease = local.split('.')[-1][:-1]
        if int(latestRelease) > int(localRelease) or float(latestVersion) > float(localVersion):
            install = self.binaries.cocoa.msgbox(title = PROGRAM, 
                text = 'Updates Available!',
                informative_text = 'YouTube-DL has an available update (%s.%s)' % (latestVersion, latestRelease), 
                button1 = 'Install', 
                button2 = 'Ignore')
            if int(install[0]) == 1:
                self.log.info('updating youtube-dl (%s.%s) to (%s.%s)' % (localVersion, localRelease, latestVersion, latestRelease))
                bar = ProgressBar(title = PROGRAM, text = 'Updating YouTube-DL...')
                bar.update(float(100.0))
                self.utils.runProcess('rm -rf %s' % self.utils.formatConsole(self.binaries.youtube_dl))
                self.utils.runProcess('curl %s -o %s && chmod a+x %s' % (rg3['versions'][latest]['bin'][0],
                    self.utils.formatConsole(self.binaries.youtube_dl), self.utils.formatConsole(self.binaries.youtube_dl)))
                bar.finish()
                self.binaries.notifier.notification(title = PROGRAM, 
                    subtitle = 'Updated YouTube-DL',
                    message = 'You have been updated from (%s.%s) to (%s.%s)' % (localVersion, localRelease, latestVersion, latestRelease),
                    sound = 'Purr',
                    sender = self.binaries.sender)
        else:
            self.binaries.cocoa.msgbox(title = PROGRAM, 
                text = 'No Updates Available', 
                informative_text = u'\u2665\tThanks for checking though!', 
                button1 = 'Ok')


"""
.. py:class Advanced()
Used to automate advanced downloads [WIP].
"""
class Advanced():

    """
    .. py:function __init__(self)
    Initialize the Advanced object.
    """
    def __init__(self):
        self.log      = Logger('advanced')
        self.utils    = Utils()
        self.binaries = Binaries()
        StartUp().startUp()
        self.download = pickle.load(open(PICKLE, 'rb'))
        if self.download.mediaFormats == None:
            self.download.mediaFormats = {}
            for i in self.utils.runProcess('%s -F %s' % (self.utils.formatConsole(self.binaries.youtube_dl), self.download.mediaurl)).split('\n')[:-1]:
                if self.utils.isInt(i[0]):
                    i = i.split()
                    dash = False
                    try: 
                        if 'dash' in i[3].lower(): dash = True 
                    except IndexError: pass
                    if 'audio' in i[2].lower():
                        new_format = u'\u3010 Audio \u3011%s %s' % (u'\u3010 DASH \u3011' if dash else '', i[1])
                    else:
                        new_format = u'\u3010 Video \u3011%s %s %s' % (u'\u3010 DASH \u3011' if dash else '', i[1], i[2])
                    self.download.mediaFormats[new_format] = i[0]
                    self.log.info('adding format (%s) pointed to (%s)' % (new_format, i[0]))
            for i in self.binaries.typeAudio:
                new_format = u'\u3010 Audio \u3011 %s' % i
                self.download.mediaFormats[u'\u3010 Audio \u3011 %s' % i] = i
                self.log.info('adding format (%s) pointed to (%s)' % (new_format, i))
            pickle.dump(self.download, open(PICKLE, 'wb'))

    """
    .. py:function advanced(self, query)
    Luxinate to Alfred advanced interaction.

    :param str query: Query to match against available formats
    """
    def advanced(self, query):
        feed = Feedback()
        for i in self.download.mediaFormats.keys():
            if len(query) > 0:
                if query.lower() in i:
                    feed.addItem(i, self.download.mediaInfo['fulltitle'], self.download.mediaFormats[i], '', '', '%s_%s.png' % (self.binaries.icons, 'video' if 'video' in i.lower() else 'audio'))
            else:
                feed.addItem(i, self.download.mediaInfo['fulltitle'], self.download.mediaFormats[i], '', '', '%s_%s.png' % (self.binaries.icons, 'video' if 'video' in i.lower() else 'audio'))
        if len(feed.feedback) <= 0:
            feed.addItem('Download Option Unavailable', 'Sorry, but "%s" isn\'t an available download option...' % query, '', '', '', '%s_x.png' % self.binaries.icons)
        return feed

    """
    .. py:function advancedDownload(self, option)
    Automate the advanced download.

    :param str option: Advanced download option (either video or audio format)
    """
    def advancedDownload(self, option):
        self.binaries.config.addHistoryEntry(self.download.mediaInfo['fulltitle'], self.download.mediaurl)
        tempLux = tempfile.mkstemp(dir = TMP, prefix = 'tmp_')[1]
        self.utils.runProcess('rm -rf %s' % tempLux)
        if self.utils.isInt(option):
            if self.utils.getMediaType(self.download.mediaInfo['_filename']) == 1:
                downloadProc = 'cd %s;%s -itf %s %s' % (self.utils.formatConsole(self.binaries.config.getDownloadDir()),
                    self.utils.formatConsole(self.binaries.youtube_dl), option, self.download.mediaurl)
                convertProc = ''
            elif self.utils.getMediaType(self.download.mediaInfo['_filename']) == 2:
                self.log.critical('invalid conversion attempted (filetype=%s option=%s)' % (
                    self.utils.getMediaType(self.download.mediaInfo['_filename']),
                    option))
                sys.exit(0)
        else:
            if self.utils.getMediaType(self.download.mediaInfo['_filename']) == 1 or (
                self.utils.getMediaType(self.download.mediaInfo['_filename']) == 2 and (
                    option.lower() not in os.path.splitext(self.download.mediaInfo['_filename'][1].lower()))):
                downloadProc = '%s -i %s -o %s' % (self.utils.formatConsole(self.binaries.youtube_dl),
                    self.download.mediaurl,
                    tempLux)
                if '.mp3' in option.lower():
                    convertProc = '%s -y -i %s -b:a 320k %s' % (self.utils.formatConsole(self.binaries.ffmpeg),
                        tempLux,
                        self.utils.formatConsole(self.utils.replaceExtension(
                            '%s%s' % (self.binaries.config.getDownloadDir(), self.download.mediaInfo['_filename']), option)))
                else:
                    convertProc = '%s -y -i %s %s' % (self.utils.formatConsole(self.binaries.ffmpeg), 
                    tempLux, 
                    self.utils.formatConsole(self.utils.replaceExtension(
                        '%s%s' % (self.binaries.config.getDownloadDir(), self.download.mediaInfo['_filename']), option)))
            elif self.utils.getMediaType(self.download.mediaInfo['_filename']) == 2:
                if option.lower() in os.path.splitext(self.downloads.mediaInfo['_filename'])[1].lower():
                    downloadProc = 'cd %s;%s -it %s' % (self.utils.formatConsole(self.binaries.config.getDownloadDir()),
                        self.utils.formatConsole(self.binaries.youtube_dl), self.download.mediaurl)
                    convertProc = ''
            else:
                self.log.critical('invalid conversion attempted (filetype=%s option=%s)' % (
                    self.utils.getMediaType(self.download.mediaInfo['_filename']),
                    option))
                sys.exit(0)
        if self.binaries.config.getProgressBar():
            progressDownload = Download().progressDownloadConvert(downloadProc, convertProc)
        else:
            self.binaries.notifier.notification(title = PROGRAM,
                subtitle = '► Downloading',
                sender = self.binaries.sender,
                message = self.download.mediaInfo['fulltitle'],
                sound = '')
            self.utils.runProcess(downloadProc)
            if len(convertProc) > 0:
                self.binaries.notifier.notification(title = PROGRAM,
                    subtitle = '► Converting',
                    sender = self.binaries.sender,
                    message = self.download.mediaInfo['fulltitle'],
                    sound = '')
                self.utils.runProcess(convertProc)
        if os.path.exists(tempLux):
            self.utils.runProcess('rm -rf %s' % tempLux)
        self.binaries.notifier.notification(title = PROGRAM, 
            subtitle = 'Download Complete', 
            sender = self.binaries.sender, 
            message = self.download.mediaInfo['fulltitle'], 
            sound = 'Glass')


"""
.. py:class Luxinate()
Used to perform interaction with Alfred.
"""
class Luxinate():

    """
    .. py:function __init__(self)
    Initialized the Luxinate object.
    """
    def __init__(self):
        self.log      = Logger('luxinate')
        self.utils    = Utils()
        self.binaries = Binaries()
        StartUp().startUp()

    """
    .. py:function buildTransfer(self, mediatile, mediafile, mediaurl)
    Build a transfer object for the current Luxinate object.

    :param str mediatitle: Title of the media
    :param str mediafile: Filename of the media
    :param str mediaurl: URL pointing to the media
    """
    def buildTransfer(self, mediaurl, mediaInfo):
        self.log.info('building transfer object at (%s)' % PICKLE)
        pickle.dump(PickleTransfer(mediaurl, mediaInfo), open(PICKLE, 'wb'))

    """
    .. py:function hasConnection(self)
    Check if the current user has a valid internet connection.
    """
    def hasConnection(self):
        try:
            urllib2.urlopen('http://www.google.com', timeout = 1)
            return True
        except urllib2.URLError:
            pass
        return False

    """
    .. py:function validUrl(self, url)
    Check if the url provided is a valid url.

    :param str url: URL provided
    """
    def validUrl(self, url):
        if 'http' in urlparse.urlparse(url).scheme:
            try:
                urllib2.urlopen(url, timeout = 2)
                return True
            except urllib2.URLError:
                pass
        return False

    """
    .. py:function supportedUrl(self, url)
    Check if the url provided is supported by youtube-dl.

    :param str url: URL provided
    """
    def supportedUrl(self, url, default = False):
        try:
            self.log.info('validating url (%s) is supported' % url)
            extractors = self.utils.runProcess('%s --list-extractors' % self.utils.formatConsole(self.binaries.youtube_dl)).split('\n')[:-1]
            url = urlparse.urlparse(url)
            test1 = url.hostname.split('.')[1]
            test2 = '%s%s' % (test1, url.path.replace('/', ':'))
            for i in extractors:
                if test1.lower() in i.lower() or test2.lower() in i.lower():
                    return True
            return False
        except AttributeError:
            return False

    """
    .. py:function getMediaInfo(self, url)
    Retrieve the mediatitle and mediafilename from a supported url.

    :param str url: URL provided
    """
    def getMediaInfo(self, url):
        try:
            return json.loads(self.utils.runProcess('%s --playlist-start 1 --playlist-end 1 -j %s' % (
                self.utils.formatConsole(self.binaries.youtube_dl), url)).replace('\n', ''))
        except:
            return False

    """
    .. py:function quickLook(self)
    Used to send the frontmost URL to Luxinate when the hotkey is activated.
    """
    def quickLook(self):
        frontmostApp = self.utils.runOsascript('return name of (info for (path to frontmost application))').replace('\n', '').split('.app')[0]
        self.log.info('gathering frontmost quicklook info for (%s)' % frontmostApp.lower())
        if 'firefox' in frontmostApp.lower():
            self.utils.runOsascript('tell application "Alfred 2" to search "luxinate ► " & "%s"' % str(self.utils.getFirefoxLast()))
            # self.binaries.cocoa.msgbox(title = PROGRAM, text = 'Firefox Unsupported', 
            #     button1 = 'Ok', informative_text = 'Sorry, but currently Firefox doesn\'t allow Luxinate to monitor URLs')
        elif 'google chrome' in frontmostApp.lower():
            available = self.utils.runOsascript('tell application "Google Chrome" to return URL of active tab of front window').replace('\n', '')
            self.utils.runOsascript('tell application "Alfred 2" to search "luxinate ► " & "%s"' % available)
        elif 'safari' in frontmostApp.lower():
            available = self.utils.runOsascript('tell application "Safari" to return URL of front document').replace('\n', '')
            self.utils.runOsascript('tell application "Alfred 2" to search "luxinate ► " & "%s"' % available)
        else:
            pass

    """
    .. py:function default(self, url)
    Default Luxinate to Alfred interaction with feed.

    :param str url: URL provided
    """
    def default(self, url):
        feed = Feedback()
        if self.hasConnection():
            if self.validUrl(url) and self.supportedUrl(url):
                try:
                    mediaInfo = self.getMediaInfo(url)
                    mediatitle = mediaInfo['fulltitle']
                    mediafile = mediaInfo['_filename']
                    self.buildTransfer(url, mediaInfo)
                    mediaType = self.utils.getMediaType(mediafile)
                    if mediaType == 1:
                        feed.addItem('Download Video', mediatitle, '1', '', '', '%s_video.png' % self.binaries.icons)
                        feed.addItem('Download Audio', mediatitle, '2', '', '', '%s_audio.png' % self.binaries.icons)
                        feed.addItem('Download Video and Audio', mediatitle, '3', '', '', '%s_both.png' % self.binaries.icons)
                    elif mediaType == 2:
                        feed.addItem('Download Audio', mediatitle, '2', '', '', '%s_audio.png' % self.binaries.icons)
                    else:
                        feed.addItem('Unknown Media Type', 'Please report "%s" to author' % url, '', '', '', '%s_x.png' % self.binaries.icons)
                except ValueError:
                    feed.addItem('Unknown Value Error', 'Report "%s" to author' % url, '', '', '', '%s_x.png' % self.binaries.icons)
            else:
                feed.addItem('Invalid URL', 'No available downloads from "%s"' % url, '', '', '', '%s_x.png' % self.binaries.icons)
        else:
            feed.addItem('No Internet Connection', 'Please check your internet connectivity', '', '', '', '%s_x.png' % self.binaries.icons)
        return feed

    """
    .. py:function playlist(self, url)
    Playlist Luxinate to Alfred interaction with feed.

    :param str url: URL to playlist provided
    """
    def playlist(self, url):
        feed = Feedback()
        if self.hasConnection():
            if self.validUrl(url) and self.supportedUrl(url):
                try:
                    mediaInfo = self.getMediaInfo(url)
                    mediatitle = mediaInfo['fulltitle']
                    mediafile = mediaInfo['_filename']
                    self.buildTransfer(url, mediaInfo)
                    mediaType = self.utils.getMediaType(mediafile)
                    if mediaType == 1:
                        feed.addItem('Download Playlist\'s Video', url, '1', '', '', '%s_video.png' % self.binaries.icons)
                        feed.addItem('Download Playlist\'s Audio', url, '2', '', '', '%s_audio.png' % self.binaries.icons)
                    elif mediaType == 2:
                        feed.addItem('Download Playlist\'s Audio', url, '2', '', '', '%s_audio.png' % self.binaries.icons)
                    else:
                        feed.addItem('Unknown Media Type', 'Please report "%s" to author' % url, '', '', '', '%s_x.png' % self.binaries.icons)
                except ValueError:
                    feed.addItem('Unkown Value Error', 'Report "%s" to author' % url, '', '', '', '%s_x.png' % self.binaries.icons)
            else:
                feed.addItem('Invalid URL', 'No available downloads from "%s"' % url, '', '', '', '%s_x.png' % self.binaries.icons)
        else:
            feed.addItem('No Internet Connection', 'Please check your internet connectivity', '', '', '', '%s_x.png' % self.binaries.icons)
        return feed 

    """
    .. py:function user(self, url)
    User Luxinate to Alfred interaction with feed.

    :param str url: URL to user provided
    """
    def user(self, url):
        feed = Feedback()
        if self.hasConnection():
            if self.validUrl(url) and self.supportedUrl(url):
                try:
                    mediaInfo = self.getMediaInfo(url)
                    mediatitle = mediaInfo['fulltitle']
                    mediafile = mediaInfo['_filename']
                    self.buildTransfer(url, mediaInfo)
                    mediaType = self.utils.getMediaType(mediafile)
                    if mediaType == 1:
                        feed.addItem('Download User\'s Video', url, '1', '', '', '%s_video.png' % self.binaries.icons)
                        feed.addItem('Download User\'s Audio', url, '2', '', '', '%s_audio.png' % self.binaries.icons)
                    elif mediaType == 2:
                        feed.addItem('Download User\'s Audio', url, '2', '', '', '%s_audio.png' % self.binaries.icons)
                    else:
                        feed.addItem('Unknown Media Type', 'Please report "%s" to author' % url, '', '', '', '%s_x.png' % self.binaries.icons)
                except ValueError:
                    feed.addItem('Unkown Value Error', 'Report "%s" to author' % url, '', '', '', '%s_x.png' % self.binaries.icons)
            else:
                feed.addItem('Invalid URL', 'No available downloads from "%s"' % url, '', '', '', '%s_x.png' % self.binaries.icons)
        else:
            feed.addItem('No Internet Connection', 'Please check your internet connectivity', '', '', '', '%s_x.png' % self.binaries.icons)
        return feed


"""
.. py:class Download()
Used to automate downloads.
"""
class Download():

    """
    .. py:function __init__(self)
    Initialize the Download object.
    """
    def __init__(self):
        self.log      = Logger('download')
        self.utils    = Utils()
        self.binaries = Binaries()
        self.download = pickle.load(open(PICKLE, 'rb'))

    """
    .. py:function defaultDetermine(self, query)
    Determine what default download to commit from the passed query.

    :param str query: Passed number to direct to desired download
    """
    def defaultDetermine(self, query):
        query = int(query)
        if query == 1:
            self.defaultVideo()
        elif query == 2:
            self.defaultAudio()
        elif query == 3:
            self.defaultVideo_Audio()

    """
    .. py:function multiDetermine(self, query)
    Determine what multi download to commit from the passed query.

    :param str query: Passed number to direct to desired download
    """
    def multiDetermine(self, query):
        query = int(query)
        if query == 1:
            self.multiVideo()
        elif query == 2:
            self.multiAudio()
        else:
            pass

    """
    .. py:function progressDownload(self, downloadProc, override=False)
    Run the download process using a progress bar.

    :param str downloadProc: Desired download process
    """
    def progressDownload(self, downloadProc, override = False, carryon = False):
        self.log.info('beginning progress download using downloadProc (%s)' % downloadProc)
        bar = ProgressBar(title = PROGRAM, text = 'Preparing Download...')
        proc = subprocess.Popen(['%s --newline' % downloadProc], stdout = subprocess.PIPE, shell = True)
        title = self.download.mediaInfo['fulltitle'] if not override else override
        for i in iter(proc.stdout.readline, ''):
            restdout = re.findall(r'[\w\']+', i)
            destdout = re.findall(r'[-+]?\d*\.\d+|\d+', i)
            if 'download' in restdout[0].lower() and 'destination' not in restdout[1].lower() and len(destdout) > 2:
                bar.update(float(destdout[0]), text = '[ETA %s:%s] %s' % (destdout[-2], destdout[-1], title))
        if not carryon:
            bar.update(float(100.0), text = 'Finishing...')
            time.sleep(0.5)
            bar.finish()
        else:
            return bar

    """
    .. py:function progressConvert(self, convertProc, override=False, carryon=False)
    Run the convert process with a progress bar.

    :param str convertProc: Desired convert process
    """
    def progressConvert(self, convertProc, override = False, carryon = False, iteration = False):
        tempLux = tempfile.mkstemp(prefix = 'ffmpeg-', dir = TMP, suffix = '.prog')[1]
        self.log.info('beginning progress convert using convertProc(%s -y >%s 2>&1)' % (convertProc, self.utils.formatConsole(tempLux)))
        if not carryon:
            bar = ProgressBar(title = PROGRAM, text = 'Preparing Convert...')
        else:
            bar = carryon
        proc = subprocess.Popen(['%s -y >%s 2>&1' % (convertProc, self.utils.formatConsole(tempLux))], stdout = subprocess.PIPE, shell = True)
        title = self.download.mediaInfo['fulltitle'] if not override else override
        with open(tempLux, 'r') as ffmpeg_prog:
            ffmpeg_prog_duration = None
            ffmpeg_prog_current = None
            while True:
                ffmpeg_prog_w = ffmpeg_prog.tell()
                ffmpeg_prog_l = ffmpeg_prog.readline()
                if not ffmpeg_prog_l:
                    time.sleep(0.05)
                    ffmpeg_prog.seek(ffmpeg_prog_w)
                else:
                    if re.findall(r'(Duration: [0-9:.]*)', ffmpeg_prog_l.replace('\n', '')):
                        prog_duration = time.strptime(re.findall(r'(Duration: [0-9:.]*)', 
                            ffmpeg_prog_l.replace('\n', ''))[0].split(': ')[1].split('.')[0], '%H:%M:%S')
                        ffmpeg_prog_duration = datetime.timedelta(
                            hours = prog_duration.tm_hour, 
                            minutes = prog_duration.tm_min, 
                            seconds = prog_duration.tm_sec).total_seconds()
                    elif re.findall(r'(time=[0-9:.]*)', ffmpeg_prog_l.replace('\n', '')):
                        prog_current = time.strptime(re.findall(r'(time=[0-9:.]*)', 
                            ffmpeg_prog_l.replace('\n', ''))[0].split('=')[1] .split('.')[0], '%H:%M:%S')
                        ffmpeg_prog_current = datetime.timedelta(
                            hours = prog_current.tm_hour,
                            minutes = prog_current.tm_min, 
                            seconds = prog_current.tm_sec).total_seconds()
                        bar.update((ffmpeg_prog_current / ffmpeg_prog_duration) * 100, '[Converting] %s' % title) 
                    elif re.findall(r'video:(.?)', ffmpeg_prog_l.replace('\n', '')):
                        break
        self.utils.runProcess('rm -rf %s' % self.utils.formatConsole(tempLux))
        if not iteration:
            bar.update(float(100.0), 'Finishing...')
            time.sleep(0.8)
            bar.finish()
        else:
            return bar

    """
    .. py:function progressDownloadConvert(self, downloadProc, convertProc)
    Run both the download and convert process using a progress bar.

    :param str downloadProc: Desired download process
    :param str convertProc: Desired convert process
    """
    def progressDownloadConvert(self, downloadProc, convertProc, override = False):
        self.log.info('beginning progress download convert using downloadProc (%s) convertProc (%s)' % (downloadProc, convertProc))
        self.progressConvert(convertProc, carryon = self.progressDownload(downloadProc, carryon = True))

    """
    .. py:function defaultVideo(self)
    Downloads the video read from self.download PICKLE object
    """
    def defaultVideo(self):
        self.binaries.config.addHistoryEntry(self.download.mediaInfo['fulltitle'], self.download.mediaurl)
        if int(self.binaries.config.getVideoOpt()) != -1:
            self.log.info('setting up download with video opt (%s)' % self.binaries.config.getVideoOpt())
            downloadProc = 'cd %s;%s -itf %s %s' % (
                self.utils.formatConsole(self.binaries.config.getDownloadDir()), 
                self.utils.formatConsole(self.binaries.youtube_dl), 
                self.binaries.config.getVideoOpt(), 
                self.download.mediaurl)
        else:
            self.log.info('setting up default download')
            downloadProc = 'cd %s;%s -it %s' % (
                self.utils.formatConsole(self.binaries.config.getDownloadDir()), 
                self.utils.formatConsole(self.binaries.youtube_dl), 
                self.download.mediaurl)
        if self.binaries.config.getProgressBar():
            self.log.info('download to be run with progressbar')
            self.progressDownload(downloadProc)
        else:
            self.binaries.notifier.notification(
                title = PROGRAM, 
                subtitle = '► Downloading Video', 
                sender = self.binaries.sender,
                message = self.download.mediaInfo['fulltitle'], 
                sound = '')
            self.utils.runProcess(downloadProc)
        self.binaries.notifier.notification(
            title = PROGRAM, 
            subtitle = 'Download Complete', 
            sender = self.binaries.sender,
            message = self.download.mediaInfo['fulltitle'], 
            sound = 'Glass')

    """
    .. py:function defaultAudio(self)
    Downloads (and converts) the audio read from self.download PICKLE object
    """
    def defaultAudio(self):
        self.binaries.config.addHistoryEntry(self.download.mediaInfo['fulltitle'], self.download.mediaurl)
        passConvert = False
        if os.path.splitext(self.download.mediaInfo['_filename'])[1].lower() in self.binaries.typeAudio and os.path.splitext(
            self.download.mediaInfo['_filename'])[1].lower() in self.binaries.config.getAudioOpt():
            self.log.info('setting up download with no conversion for (%s)' % self.download.mediaInfo['_filename'])
            passConvert = True
        tempLux = tempfile.mkstemp(dir = TMP, prefix = 'tmp_')[1]
        self.utils.runProcess('rm -rf %s' % tempLux)
        if not passConvert:
            downloadProc = '%s -i %s -o %s' % (
                self.utils.formatConsole(self.binaries.youtube_dl), 
                self.download.mediaurl, 
                tempLux)
            if '.mp3' in self.binaries.config.getAudioOpt():
                self.log.info('setting up conversion of (.mp3) for (%s)' % self.download.mediaInfo['_filename'])
                convertProc = '%s -y -i %s -b:a 320k %s' % (
                    self.utils.formatConsole(self.binaries.ffmpeg), 
                    tempLux, 
                    self.utils.formatConsole(self.utils.replaceExtension('%s%s' % (
                            self.binaries.config.getDownloadDir(), 
                            self.download.mediaInfo['_filename']), 
                        self.binaries.config.getAudioOpt())))
            else:
                self.log.info('setting up conversion of (%s) for (%s)' % (
                    self.binaries.config.getAudioOpt(), 
                    self.download.mediaInfo['_filename']))
                convertProc = '%s -y -i %s %s' % (
                    self.utils.formatConsole(self.binaries.ffmpeg), 
                    tempLux, 
                    self.utils.formatConsole(
                        self.utils.replaceExtension('%s%s' % (
                            self.binaries.config.getDownloadDir(), 
                            self.download.mediaInfo['_filename']),
                        self.binaries.config.getAudioOpt())))
        else:
            downloadProc = 'cd %s;%s -it %s' % (
                self.utils.formatConsole(self.binaries.config.getDownloadDir()), 
                self.utils.formatConsole(self.binaries.youtube_dl), 
                self.download.mediaurl)
            convertProc = ''
        if self.binaries.config.getProgressBar():
            self.log.info('download to be run with progressbar')
            if not passConvert:
                self.log.info('convert to be run with progressbar')
                self.progressDownloadConvert(downloadProc, convertProc)
            else:
                self.progressDownload(downloadProc)
        else:
            self.binaries.notifier.notification(
                title = PROGRAM, 
                subtitle = '► Downloading Audio', 
                sender = self.binaries.sender,
                message = self.download.mediaInfo['fulltitle'], 
                sound = '')
            self.utils.runProcess(downloadProc)
            if not passConvert:
                self.utils.runProcess(convertProc)
        self.utils.runProcess('rm -rf %s' % tempLux)
        self.binaries.notifier.notification(
            title = PROGRAM, 
            subtitle = 'Download Complete', 
            sender = self.binaries.sender, 
            message = self.download.mediaInfo['fulltitle'], 
            sound = 'Glass')

    """
    .. py:function defaultVideo_Audio(self)
    Downloads the video and converts to audio from read from self.download PICKLE object.
    """
    def defaultVideo_Audio(self):
        self.binaries.config.addHistoryEntry(self.download.mediaInfo['fulltitle'], self.download.mediaurl)
        if int(self.binaries.config.getVideoOpt()) != -1:
            self.log.info('setting up download with video opt (%s)' % self.binaries.config.getVideoOpt())
            downloadProc = 'cd %s;%s -itf %s %s' % (
                self.utils.formatConsole(self.binaries.config.getDownloadDir()), 
                self.utils.formatConsole(self.binaries.youtube_dl), 
                self.binaries.config.getVideoOpt(), 
                self.download.mediaurl)
        else:
            self.log.info('setting up default download')
            downloadProc = 'cd %s;%s -it %s' % (
                self.utils.formatConsole(self.binaries.config.getDownloadDir()), 
                self.utils.formatConsole(self.binaries.youtube_dl), 
                self.download.mediaurl)
        if '.mp3' in self.binaries.config.getAudioOpt():
            convertProc = '%s -y -i %s -b:a 320k %s' % (
                self.utils.formatConsole(self.binaries.ffmpeg), 
                self.utils.formatConsole('%s%s' % (
                    self.binaries.config.getDownloadDir(), 
                    self.download.mediaInfo['_filename'])),
                self.utils.formatConsole(
                    self.utils.replaceExtension('%s%s' % (
                        self.binaries.config.getDownloadDir(), 
                        self.download.mediaInfo['_filename']), 
                    self.binaries.config.getAudioOpt())))
        else:
            convertProc = '%s -y -i %s %s' % (
                self.utils.formatConsole(self.binaries.ffmpeg), 
                self.utils.formatConsole('%s%s' % (
                    self.binaries.config.getDownloadDir(), 
                    self.download.mediaInfo['_filename'])),
                self.utils.formatConsole(self.utils.replaceExtension('%s%s' % (
                    self.binaries.config.getDownloadDir(), 
                    self.download.mediaInfo['_filename']), 
                self.binaries.config.getAudioOpt())))
        if self.binaries.config.getProgressBar():
            self.progressDownloadConvert(downloadProc, convertProc)
        else:
            self.binaries.notifier.notification(
                title = PROGRAM, 
                subtitle = '► Downloading Video', 
                sender = self.binaries.sender,
                message = self.download.mediaInfo['fulltitle'], 
                sound = '')
            self.utils.runProcess(downloadProc)
            self.binaries.notifier.notification(
                title = PROGRAM, 
                subtitle = '► Downloading Audio', 
                sender = self.binaries.sender, 
                message = self.download.mediaInfo['fulltitle'], 
                sound = '')
            self.utils.runProcess(convertProc)
        self.binaries.notifier.notification(
            title = PROGRAM, 
            subtitle = 'Download Complete', 
            sender = self.binaries.sender, 
            message = self.download.mediaInfo['fulltitle'], 
            sound = 'Glass')

    """
    .. py:function multiVideo(self)
    Download a multi-sequence list of videos returned by youtube-dl.
    """
    def multiVideo(self):
        if int(self.binaries.config.getVideoOpt()) != -1:
            self.log.info('setting up multi download with video opt (%s)' % self.binaries.config.getVideoOpt())
            downloadProc = 'cd %s;%s -itf %s %s' % (
                self.utils.formatConsole(self.binaries.config.getDownloadDir()), 
                self.utils.formatConsole(self.binaries.youtube_dl),
                self.binaries.config.getVideoOpt(), 
                self.download.mediaurl)
        else:
            self.log.info('setting up default multi download')
            downloadProc = 'cd %s;%s -it %s' % (
                self.utils.formatConsole(self.binaries.config.getDownloadDir()), 
                self.utils.formatConsole(self.binaries.youtube_dl),
                self.download.mediaurl)
        if self.binaries.config.getProgressBar():
            self.log.info('download to be run with progressbar')
            self.progressDownload(downloadProc, override = self.download.mediaurl)
        else:
            proc = subprocess.Popen([downloadProc], stdout = subprocess.PIPE, shell = True)
            self.binaries.config.editMultiPid(proc.pid)
            for i in iter(proc.stdout.readline, ''):
                restdout = re.findall(r'[\w\']+', i)
                if 'download' in restdout[0].lower() and 'destination' in restdout[1].lower():
                    self.binaries.config.addHistoryEntry('-'.join(i.replace('\n', '').split(': ')[1].split('-')[:-1]), self.download.mediaurl)
                    self.binaries.notifier.notification(
                        title = PROGRAM,
                        subtitle = '► Downloading Video',
                        sender = self.binaries.sender,
                        message = '-'.join(i.replace('\n', '').split(': ')[1].split('-')[:-1]),
                        sound = '')
        self.binaries.config.editMultiPid('', default = True)
        self.binaries.notifier.notification(
            title = PROGRAM, 
            subtitle = 'Download Complete', 
            sender = self.binaries.sender,
            message = self.download.mediaurl, 
            sound = 'Glass')

    """
    .. py:function multiAudio(self)
    Download a multi-sequence list of audio returned by youtube-dl.
    """
    def multiAudio(self):
        passConvert = False
        if os.path.splitext(self.download.mediaInfo['_filename'])[1].lower() in self.binaries.typeAudio and os.path.splitext(
            self.download.mediaInfo['_filename'])[1].lower() in self.binaries.config.getAudioOpt():
            self.log.info('setting up multi download with no conversion for (%s)' % self.download.mediaInfo['_filename'])
            passConvert = True
        tempLux = tempfile.mkdtemp(dir = TMP, prefix = 'tmp_')
        if not passConvert:
            downloadProc = 'cd %s;%s -it %s' % (
                self.utils.formatConsole(tempLux), 
                self.utils.formatConsole(self.binaries.youtube_dl), 
                self.download.mediaurl)
        else:
            downloadProc = 'cd %s;%s -it %s' % (
                self.utils.formatConsole(self.binaries.config.getDownloadDir()), 
                self.utils.formatConsole(self.binaries.youtube_dl), 
                self.download.mediaurl)
        if self.binaries.config.getProgressBar():
            self.log.info('download to be run with progressbar')
            if not passConvert:
                self.log.info('convert to be run with progressbar')
            self.progressDownload(downloadProc, override = self.download.mediaurl)
        else:
            proc = subprocess.Popen([downloadProc], stdout = subprocess.PIPE, shell = True)
            self.binaries.config.editMultiPid(proc.pid)
            for i in iter(proc.stdout.readline, ''):
                restdout = re.findall(r'[\w\']+', i)
                if 'download' in restdout[0].lower() and 'destination' in restdout[1].lower():
                    self.binaries.config.addHistoryEntry('-'.join(i.replace('\n', '').split(': ')[1].split('-')[:-1]), self.download.mediaurl)
                    self.binaries.notifier.notification(
                        title = PROGRAM,
                        subtitle = '► Downloading Audio',
                        sender = self.binaries.sender,
                        message = '-'.join(i.replace('\n', '').split(': ')[1].split('-')[:-1]),
                        sound = '')
        if not passConvert:
            bar = None
            for i in os.listdir(tempLux):
                if '.mp3' in self.binaries.config.getAudioOpt():
                    self.log.info('setting up conversion of (.mp3) for (%s)' % i)
                    convertProc = '%s -y -i %s -b:a 320k %s' % (
                        self.utils.formatConsole(self.binaries.ffmpeg), 
                        self.utils.formatConsole('%s/%s' % (tempLux, i)), 
                        self.utils.formatConsole(self.utils.replaceExtension('%s/%s' % (tempLux, i), 
                        self.binaries.config.getAudioOpt())))
                else:
                    self.log.info('setting up conversion of (%s) for (%s)' % (self.binaries.config.getAudioOpt(), i))
                    convertProc = '%s -y -i %s %s' % (
                        self.utils.formatConsole(self.binaries.ffmpeg), 
                        self.utils.formatConsole('%s/%s' % (tempLux, i)), 
                        self.utils.formatConsole(self.utils.replaceExtension('%s/%s' % (tempLux, i), 
                        self.binaries.config.getAudioOpt())))
                if self.binaries.config.getProgressBar():
                    if bar != None:
                        bar = self.progressConvert(convertProc, override = self.download.mediaurl, carryon = bar, iteration = True)
                    else:
                        bar = self.progressConvert(convertProc, override = self.download.mediaurl, iteration = True)
                else:
                    self.binaries.notifier.notification(
                        title = PROGRAM,
                        subtitle = '► Converting Audio',
                        sender = self.binaries.sender,
                        message = os.path.splitext(i)[0],
                        sound = '')
                    self.utils.runProcess(convertProc)
                self.utils.runProcess('rm -rf %s' % self.utils.formatConsole('%s/%s' % (tempLux, i)))
            for i in os.listdir(tempLux):
                self.utils.runProcess('mv %s %s' % (self.utils.formatConsole('%s/%s' % (tempLux, i)), 
                    self.utils.formatConsole('%s%s' % (self.binaries.config.getDownloadDir(), i))))
        self.utils.runProcess('rm -rf %s' % self.utils.formatConsole(tempLux))
        self.binaries.config.editMultiPid('', default = True)
        self.binaries.notifier.notification(
            title = PROGRAM, 
            subtitle = 'Download Complete', 
            sender = self.binaries.sender,
            message = self.download.mediaurl, 
            sound = 'Glass')


"""
.. py:class ProgressBar()
Used to manage a progress bar spawned from cocoadialog.
"""
class ProgressBar():

    """
    .. py:function __init__(self, title='', text='', percent='0')
    Initialize and spawn the progress bar.

    :param str title: Title of progress bar
    :param str text: Subtitle of progress bar
    :param str percent: Current percent present of progress bar
    """
    def __init__(self, title = '', text = '', percent = '0'):
        self.log = Logger('progressbar')
        self.log.info('setting up progress bar object at (%s)' % self)
        self.utils = Utils()
        self.binaries = Binaries()
        self.title = title
        self.text = text
        self.percent = percent
        self.pipe = os.popen('%s progressbar --title "%s" --text "%s" --percent "%s" --icon-file "%s"' % (
            self.utils.formatConsole(self.binaries.cocoa.cocoa),
            self.title,
            self.text,
            self.percent, 
            '%sicon.png' % self.binaries.workflow), 'w')

    """
    .. py:function update(self, percent, text=False)
    Update the spawned progress bar object.

    :param str percent: New percent of progress bar
    :param bool text: No change text if false, else edit to new string passed through text
    """
    def update(self, percent, text = False):
        if text:
            self.text = text
        self.pipe.write('%f %s\n' % (percent, self.text))
        self.pipe.flush()

    def finish(self):
        self.pipe.close()
        

"""
.. py:class Cocoa()
Used to display cocoa dialogs from Luxinate.
"""
class Cocoa():

    """
    .. py:function __init__(self, cocoa)
    Initialize the Cocoa object.

    :param str cocoa: Path to cocoadialog binary
    """
    def __init__(self, cocoa):
		self.log = Logger('cocoa')
		self.utils = Utils()
		self.cocoa = cocoa

    """
    .. py:function displayCocoa(self, funct, args, values)
    Spawn the cocoa dialog passed.

    :param str funct: Function name of cocoa dialog
    :param list args: List of arguments of cocoa dialog
    :param list values: List of values of cocoa dialog
    """
    def displayCocoa(self, funct, args, values):
        process = '%s %s' % (self.cocoa, funct.replace('_', '-'))
        for i in args[1:]:
            if (isinstance(values[i], str) or isinstance(values[i], unicode) or isinstance(values[i], list)) and len(values[i]) > 0:
                if isinstance(values[i], list):
                    values[i] = '" "'.join(values[i])
                process = '%s --%s "%s"' % (process, i.replace('_', '-'), values[i])
            elif isinstance(values[i], bool) and values[i]:
                process = '%s --%s' % (process, i.replace('_', '-'))
            else:
                pass
        try:
            return self.utils.runProcess(process).split('\n')[:-1]
        except IndexError:
            return False

    def checkbox(self, button1 = '', button2 = '', button3 = '', cancel = False, checked = [], columns = '', debug = False, disabled = [], height = '', icon = '', 
        icon_file = '', icon_height = '', icon_size = '', icon_type = '', icon_width = '', items = [], label = '', minimize = False, mixed = False, no_float = False,
        no_newline = False, posX = '', posY = '', quiet = False, resize = False, rows = '', string_output = False, timeout = '', timeout_format = '', title = '',
        value_required = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(items) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (items)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)
    
    def msgbox(self, text = '', button1 = '', button2 = '', button3 = '', cancel = False, debub = False, empty_text = '', height = '', icon = '',
        icon_file = '', icon_height = '', icon_size = '', icon_type = '', icon_width = '', label = '', minimize = False, no_float = False, no_newline = False,
        posX = '', posY = '', quiet = False, resize = False, string_output = False, timeout = '', timeout_format = '', title = '', value_required = '', width = '',
        informative_text = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(text) <= 0 or len(informative_text) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (text) and (informative_text)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def dropdown(self, button1 = '', button2 = '', button3 = '', cancel = False, debug = False, empty_text = '', exit_onchange = False, height = '', icon = '',
        icon_file = '', icon_height = '', icon_size = '', icon_type = '', icon_width = '', items = [], label = '', minimize = False, no_float = False, no_newline = False,
        posX = '', posY = '', pulldown = False, quiet = False, resize = False, selected = '', string_output = False, timeout = '', timeout_format = '', title = '',
        value_required = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(items) <= 0 or len(label) <=0 :
            self.log.warning('must provide values for (button1) and (title) and (items) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def filesave(self, debug = False, height = '', icon = '', icon_file = '', icon_height = '', icon_size = '', icon_type = '', icon_width = '', label = '', 
        minimize = False, no_create_directories = False, no_float = False, no_newline = False, packages_as_directories = False, posX = '', posY = '', 
        quiet = False, resize = False, string_output = False, timeout = '', timeout_format = '', title = '', width = '', with_directory = '', 
        with_extensions = [], with_file = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def fileselect(self, debug = False, height = '', icon = '', icon_file = '', icon_height = '', icon_size = '', icon_type = '', icon_width = '', label = '',
        minimize = False, no_float = False, no_newline = False, no_select_directories = False, no_select_multiple = False, packages_as_directories = False, 
        posX = '', posY = '', quiet = False, resize = False, select_directories = False, select_multiple = False, select_only_directories = False, 
        string_output = False, timeout = '', timeout_format = '', title = '', width = '', with_directory = '', with_extensions = [], with_file = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def inputbox(self, button1 = '', button2 = '', button3 = '', cancel = False, debug = False, empty_text = '', height = '', icon = '', icon_file = '', 
        icon_height = '', icon_size = '', icon_type = '', icon_width = '', label = '', minimize = False, no_float = False, no_newline = False, no_show = False,
        not_selected = False, posX = '', posY = '', quiet = False, resize = False, string_output = False, timeout = '', timeout_format = '', title = '',
        value = '', value_required = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def ok_msgbox(self, button = '', button1 = '', button2 = '', button3 = '', cancel = False, debug = False, empty_text = '', height = '', icon = '',
        icon_file = '', icon_height = '', icon_size = '', icon_type = '', icon_width = '', label = '', minimize = False, no_cancel = False, no_float = False,
        no_newline = False, posX = '', posY = '', quiet = False, resize = False, string_output = False, timeout = '', timeout_format = '', title = '', 
        value_required = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def progressbar(self, debug = False, float = False, height = '', icon = '', icon_file = '', icon_height = '', icon_size = '', icon_type = '', icon_width = '',
        indeterminate = False, minimize = False, no_float = False, no_newline = False, percent = '', posX = '', posY = '', quiet = False, resize = False, stoppable = False,
        string_output = False, text = '', timeout = '', timeout_format = '', title = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def radio(self, allow_mixed = False, button1 = '', button2 = '', button3 = '', cancel = False, columns = '', debug = False, disabled = [], empty_text = '',
        height = '', icon = '', icon_file = '', icon_height = '', icon_size = '', icon_type = '', icon_width = '', items = [], label = '', minimize = False, 
        mixed = False, no_float = False, no_newline = False, posX = '', posY = '', quiet = False, resize = False, rows = '', selected = '', string_output = False,
        timeout = '', timeout_format = '', title = '', value_required = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def secure_inputbox(self, button1 = '', button2 = '', button3 = '', cancel = False, debug = False, empty_text = '', height = '', icon = '', icon_file = '', 
        icon_height = '', icon_size = '', icon_type = '', icon_width = '', label = '', minimize = False, no_float = False, no_newline = False, no_show = False,
        not_selected = False, posX = '', posY = '', quiet = False, resize = False, string_output = False, timeout = '', timeout_format = '', title = '', value = '',
        value_required = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def secure_standard_inputbox(self, button1 = '', button2 = '', button3 = '', cancel = False, debug = False, empty_text = '', height = '', icon = '', icon_file = '', 
        icon_height = '', icon_size = '', icon_type = '', icon_width = '', label = '', minimize = False, no_float = False, no_newline = False, no_show = False,
        posX = '', posY = '', quiet = False, resize = False, string_output = False, timeout = '', timeout_format = '', title = '', value = '', selected = '', 
        value_required = '', width = '', no_cancel = False):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def slider(self, always_show_value = False, button1 = '', button2 = '', button3 = '', cancel = False, debug = False, empty_text = '', empty_value = '', 
        height = '', icon = '', icon_file = '', icon_height = '', icon_size = '', icon_type = '', icon_width = '', label = '', max = '', min = '', minimize = False,
        no_float = False, no_newline = False, posX = '', posY = '', quiet = False, resize = False, return_float = False, slider_label = '', string_output = False,
        ticks = '', timeout = '', timeout_format = '', title = '', value = '', value_required = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def standard_dropdown(self, button1 = '', button2 = '', button3 = '', cancel = False, debug = False, empty_text = '', exit_onchange = False, height = '',
        icon = '', icon_file = '', icon_height = '', icon_size = '', icon_type = '', icon_width = '', items = [], label = '', minimize = False, no_cancel = False,
        no_float = False, no_newline = False, posX = '', posY = '', pulldown = False, quiet = False, resize = False, selected = '', string_output = False, 
        timeout = '', timeout_format = '', title = '', value_required = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def standard_inputbox(self, button1 = '', button2 = '', button3 = '', cancel = False, debug = False, empty_text = '', height = '', icon = '', icon_file = '',
        icon_height = '', icon_size = '', icon_type = '', icon_width = '', label = '', minimize = False, no_cancel = False, no_float = False, no_newline = False,
        no_show = False, posX = '', posY = '', quiet = False, resize = False, selected = '', string_output = False, timeout = '', timeout_format = '', title = '',
        value = '', value_required = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def textbox(self, button1 = '', button2 = '', button3 = '', cancel = False, debug = False, editable = False, empty_text = '', focus_textbox = False, height = '',
        icon = '', icon_file = '', icon_height = '', icon_size = '', icon_type = '', icon_width = '', label = '', minimize = False, no_editable = False, no_float = False,
        no_newline = False, posX = '', posY = '', quiet = False, resize = False, scroll_to = '', selected = '', string_output = False, text = '', text_from_file = '',
        timeout = '', timeout_format = '', title = '', value_required = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)

    def yesno_msgbox(self, button = '', button1 = '', button2 = '', button3 = '', cancel = False, debug = False, empty_text = '', height = '', icon = '', icon_file = '',
        icon_height = '', icon_size = '', icon_type = '', icon_width = '', label = '', minimize = False, no_cancel = False, no_float = False, no_newline = False,
        posX = '', posY = '', quiet = False, resize = False, string_output = False, timeout = '', timeout_format = '', title = '', value_required = '', width = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building cocoa.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(button1) <= 0 or len(title) <= 0 or len(label) <= 0:
            self.log.warning('must provide values for (button1) and (title) and (label)')
            sys.exit(1)
        else:
            return self.displayCocoa(funct, args, values)


"""
.. py:class Notifier()
Used for displaying MacOSX notifications on platforms 6+.
"""
class Notifier():

    """
    .. py:function __init__(self, notifier)
    Initialize the Notifier object.

    :para str notifier: Path to the notifier binary (/Applications/terminal-notifier.app/Contents/MacOS/terminal-notifier)
    """
    def __init__(self, notifier):
        self.log      = Logger('notifier')
        self.utils    = Utils()
        self.notifier = notifier

	"""
	.. py:function notification(self, titile='', subtitle='', sound='default', group='', sender='', open='', execute='', message='')
	Used for building and running the notification subprocess.

	:param str title: Title of the notification
	:param str subtitle: Subtitle of the notification
	:param str sound: Sound to play from "Sound Preferences"
	:param str group: Group ID of notification
	:param str sender: Bundle identifier of sending application
	:param str open: URL to open on notification click
	:param str execute: Command to execute on notification click
	:param str message: Message of notification
	"""
    def notification(self, title = '', subtitle = '', sound = 'default', group = 'lux.info', sender = '', open = '', execute = '', message = ''):
        frame = inspect.currentframe()
        funct = inspect.getframeinfo(frame)[2]
        args, _, _, values = inspect.getargvalues(frame)
        self.log.info('building notifier.%s with arguments %s' % (funct, str([(i, values[i]) for i in args])))
        if len(title) <= 0:
            self.log.warning('must provide values for (title)')
            sys.exit(1)
        else:
            process = self.notifier
            for i in args[1:]:
                if len(values[i]) > 0:
                    if 'message' in i and '[' in values[i].decode('utf-8').encode('utf-8')[0]:
                        process = '%s -%s "%s"' % (process, i, values[i].replace('[', '\[').decode('utf-8').encode('utf-8'))
                    else:
                        process = '%s -%s "%s"' % (process, i, values[i].decode('utf-8').encode('utf-8'))
            self.utils.runProcess(process)


"""
.. py:class GenMD()
Used to generate markdown for display.
"""
class GenMD():

    """
    .. py:function __init__(self, theme='yeti')
    Initialize the markdown generation object.
    """
    def __init__(self, theme='yeti'):
        self.stylesheet   = '<link rel="stylesheet" href="http://www.bootswatch.com/%s/bootstrap.css">' % theme
        self.log          = Logger('genmd')
        self.utils        = Utils()
        self.binaries     = Binaries()
        self.remote       = 'https://raw.githubusercontent.com/Ritashugisha/Luxinate/master/Versions/'
        self.contributors = '%scontributors.json' % self.remote

    """
    .. displayMD(self, mdString):
    Display the markdown given in mdString.

    :param str mdString: String containing markdown code
    :param bool header: Genereate header if True
    """
    def displayMD(self, mdString, header=True):
        tempLux = tempfile.mkstemp(dir = TMP, prefix = 'Luxinate_Markdown-')[1]
        self.utils.runProcess('rm -rf %s' % self.utils.formatConsole(tempLux))
        mdString = '%s\n%s%s' % (self.stylesheet if header else '', self.customGenHeader() if header else '', mdString)
        with open(tempLux, 'w') as bump:
            bump.write(mdString)
        self.log.info('displaying markdown from mdString (%s)' % mdString)
        self.utils.runProcess('qlmanage -p %s -c .md -g %s 2>&1 >/dev/null' % (self.utils.formatConsole(tempLux), 
           self.utils.formatConsole(self.binaries.qlmdmgr)))
        self.utils.runProcess('rm -rf %s' % self.utils.formatConsole(tempLux))

    """
    .. py:function genImage(self, src, alt='', width='', height='')
    Generate a simple html image link.

    :param str src: SRC to image
    :param str alt: ALT of image
    :param str width: Width of image
    :param str height: Height of image
    """
    def genImage(self, src, alt='', width='', height=''):
        return '<img src="%s" alt="%s" width="%s" height="%s"/>' % (src, alt, width, height)

    """
    .. py:function genButton(self, text, size='lg', typ='defualt', block=False)
    Generate a simple html button.

    :param str text: Text of button
    :param str size: Size of button ['lg', '', 's', 'xs']
    :param str typ: Type of button
    :param bool block: Block button if True
    """
    def genButton(self, text, size='lg', typ='default', block=False):
        return '<button type="button" class="btn btn-%s btn-%s %s">%s</button>' % (typ, size, 'btn-block' if block else '', text)

    """
    .. py:function genPanel(self, title, text, title_size=4, typ='defualt')
    Generate a simple html panel.

    :param str title: Title of panel
    :param str text: Text of panel
    :param int title_size: Corresponds to h1, h2, h3...
    :param str typ: Type of panel
    """
    def genPanel(self, title, text, title_size=3, typ='default'):
        return ''.join(['<div class="panel panel-%s"><div class="panel-heading">' % typ,
            '<h%d class="panel-title">%s</h%d></div><div class="panel-body">%s</div></div>' % (title_size, title, title_size, text)])

    """
    .. py:function genButton(self, title, text, title_size=4, typ='defualt')
    Generate a simple html button.

    :param str title: Title of button
    :param str text: Text of button
    :param int title_size: Corresponds to h1, h2, h3...
    :param str typ: Type of button
    """
    def genAlert(self, title, text, title_size=4, typ='default'):
        return ''.join(['<div class="alert alert-dismissable alert-%s"><h%d>%s</h%d>' % (typ, title_size, title, title_size),
            '<p>%s</p></div>' % text])

    """
    .. py:function genBlockQuote(self, text, author, pretext='', subtext='', pull_right=False)
    Generate a simple html blockquote.

    :param str text: Text of quote
    :param str author: Author's name
    :param str pretext: Pretext before quote
    :param str subtext: Text before author's name
    :param bool pull_right: Justify right if True
    """
    def genBlockQuote(self, text, author, pretext='', subtext='', pull_right=False):
        return ''.join(['<blockquote class="%s">%s<p>%s</p><small>%s<cite class="Source Title">%s</cite></small></blockquote>' % (
            'pull-right' if pull_right else '', pretext, text, subtext, author)])

    """
    .. py:function genTable(self, table, typ=[])
    Generate a simple html table.

    :param str table: Dictionary where key is column header and value is list of values in column
    :param str typ: List of values of highlight for rows top to bottom
    """
    def genTable(self, table, typ=[]):
        retr = ['<table class="table table-striped table-hover"><thead><tr>',
            '%s</tr></thead><tbody>' % ''.join(['<th>%s</th>' % i if 'null' not in i else '' for i in table.keys()])]
        for i in range(0, max([len(j) for j in table.values()])):
            try:retr.extend(['<tr class="%s">' % typ[i]])
            except IndexError:retr.extend(['<tr>'])
            for j in range(0, len(table.values())):
                try:retr.extend(['<td>%s</td>' % table.values()[j][i]])
                except IndexError:retr.extend(['<td></td>'])
            retr.extend(['</tr>'])
        retr.extend(['</tbody></table>'])
        return ''.join(retr)

    """
    .. py:function(self, jsonLink)
    Load and return as json object from the given link.

    :param str jsonLink: Link to a json file
    """
    def jsonLoad(self, jsonLink):
        self.log.info('retrieving json from (%s)' % jsonLink)
        return json.loads(urllib.urlopen(jsonLink).read())

    """
    .. py:function genHeader(self)
    Generate the header for all Luxinate markdowns.
    """
    def customGenHeader(self):
        self.log.info('generating a markdown header')
        return '\n'.join(['#%s <small>v%sr%s</small>' % (PROGRAM, VERSION, RELEASE),
            '#####<p class="text-primary">GNU GPLv3 &#8212; %s &#169; %s</p><hr>' % (datetime.datetime.now().strftime('%Y'), AUTHOR)])

    """
    .. py:function customGenDonate(self)
    Generate the donate panel.
    """
    def customGenDonate(self):
        self.log.info('generating a markdown donate panel')
        donateLink = self.jsonLoad(self.contributors)['donate_link']
        return ''.join(['<div class="jumbotron"><h1>Donate</h1>',
            '<p class="text-muted">Want to support %s\'s continuing development?<br>Donate through <i>PayPal</i> below!</p>' % PROGRAM,
            '<a href="%s">%s</a></div>' % (donateLink, self.genButton('PayPal', block=True, typ='info'))])

    """
    .. py:function customGenContributor(self, name, description, donation, image='')
    Generate a contributor for contributors section.

    :param str name: Name of contributor
    :param str description: Description of contributor
    :param str donation: Amount donated
    :param str image: Contributor thumbnail URL
    """
    def customGenContributor(self, name, description, donation, image=''):
        return self.genBlockQuote('<p>&hearts; %s ($%s USD)</p>' % (description, donation), 
                name, pretext=self.genImage(image, alt=name, width='100', height='100'))

    """
    .. py:function customGenContributors(self)
    Generate a list of contributors.
    """
    def customGenContributors(self):
        self.log.info('generating a markdown list of contributors')
        contributors = ['<p class="lead">Contributors</p>']
        for i in self.jsonLoad(self.contributors)['contributors']:
            contributors.extend([self.customGenContributor(i['name'], i['description'], i['donation'], image=i['thumbnail'])])
        return ''.join(contributors)

    """
    .. py:function customGenSupportedDomains(self)
    Generate a table of supported domains.
    """
    def customGenSupportedDomains(self):
        self.log.info('generating a markdown list of supported domains')
        supported = [i.replace(':', '/') for i in self.utils.runProcess(
            '%s --list-extractors' % self.utils.formatConsole(self.binaries.youtube_dl)).split('\n')[:-1]]
        return ''.join(['<h2>Supported Domains</h2>', self.genTable({'Domain':supported})])

    """
    .. py:function customGenAbout(self):
    Genearte the about markdown.
    """
    def customGenAbout(self):
        customGen = [self.genBlockQuote('%s has be rebuilt to promote speed and stability for downloading from a ever growing set of supported sites.' % PROGRAM,
            'Ritashugisha', pull_right=True), self.customGenDonate(), '<h1 align="center">Hey, %s!</h1>' % getpass.getuser().title(),
            '<center><p class="lead">Thanks for downloading %s &hearts;</p></center>' % PROGRAM, 
            '<p>Remember, that if you run into any bugs or problems don\'t hesitate to ask me at `%s` or on the ' % CONTACT,
            '<a href="http://www.alfredforum.com/topic/3238-luxinate-download-video-and-audio-from-youtube-and-soundcloud/">Alfred Forum</a>.</p><hr>\n']
        customGen.append(open('%s%s' % (self.binaries.workflow, ABOUT), 'r').read())
        return ''.join(customGen)

    """
    .. py:function customGenTest(self)
    Generate test results from the current state of Luxinate.
    """
    def customGenTest(self):
        infoPanel = [self.genPanel('Program Info', self.genTable({
            'null0':['AUTHOR','CONTACT','PROGRAM','DESCRIPTION','VERSION'], 
            'null1':[AUTHOR, CONTACT, PROGRAM, DESCRIPTION, 'v%sr%s' % (VERSION, RELEASE)]}), typ='info')]
        requirementsContent = []
        if not os.path.exists(self.binaries.pkgmanager):
            requirementsContent.extend([self.genAlert('Error!', 
                'You seem to be missing %s\'s package manager.<p align="right">Please try reinstalling the workflow</p>' % PROGRAM, 
                typ='danger')])
        else:
            requirementsContent.extend([self.genAlert('Validated!', 'Found %s\'s package manager.' % PROGRAM, typ='success')])
            dependenciesTable = {'Dependency':[], 'Status':[]}
            dependenciesStatus = []
            for i in self.binaries.dependencies:
                dependenciesTable['Dependency'].append(i['title'].upper())
                if not os.path.exists(i['loci']):
                    dependenciesTable['Status'].append('Missing')
                    dependenciesStatus.append('danger')
                else:
                    dependenciesTable['Status'].append('Found')
                    dependenciesStatus.append('success')
        currentPython = float('%s.%s' % (sys.version_info[0], sys.version_info[1]))
        requiredPython = float(self.binaries.config.getRoot().find('.//python').text)
        if currentPython != requiredPython:
            requirementsContent.extend([self.genAlert('Error!', 
                'Your Python version (%s) is throwing an error.<p align="right"><a href="https://www.python.org/downloads/">Grab required version (%s)</a></p>' % (
                    str(currentPython), str(requiredPython)), typ='danger')])
        else:
            requirementsContent.extend([self.genAlert('Validated!', 'You have the correct version of Python (%s)' % requiredPython, typ='success')])
        requiredModulesTable = {'Modules':[], 'Import Status':[]}
        requiredModulesStatus = []
        for i in [v.__name__ for (k, v) in globals().items() if isinstance(v, types.ModuleType)]:
            requiredModulesTable['Modules'].append(i.upper())
            try:
                __import__(i)
                requiredModulesTable['Import Status'].append('Successfully Imported')
                requiredModulesStatus.append('success')
            except ImportError:
                requiredModulesTable['Import Status'].append('Failed to Import')
                requiredModulesStatus.append('danger')
        requirementsContent.extend(self.genTable(dependenciesTable, typ=dependenciesStatus))
        requirementsContent.extend(self.genTable(requiredModulesTable, typ=requiredModulesStatus))
        requirementsPanel = self.genPanel('Program Requirements', ''.join(requirementsContent), typ='primary')
        t1 = time.time()
        StartUp().startUp()
        timeStartUp = time.time() - t1
        t1 = time.time()
        self.utils.runProcess('%s --playlist-start 1 --playlist-end 1 -qsj https://www.youtube.com/watch?v=is0c_Q9hJrY' % self.utils.formatConsole(self.binaries.youtube_dl))
        timeMediaInfo = time.time() - t1
        timePanel = self.genPanel('StartUp Time', ''.join(self.genTable(
            {'Method':['Start Up', 'Retrieve Media Info'], 'Time (seconds)':[timeStartUp, timeMediaInfo]})), typ='success')
        return ''.join([''.join(infoPanel), ''.join(requirementsPanel), ''.join(timePanel)])

    # TODO: Cool idea... but overflows quicklook's dedicated
    #
    # def customGenLog(self):
    #     logTable = {'Type':[], 'Function':[], 'Line':[], 'Message':[]}
    #     logStatus = []
    #     with open(LOG, 'r') as rett:
    #         for i in rett:
    #             if '[' in i[0]:
    #                 calltype = i[23:31].rsplit()[0]
    #                 if 'critical' in calltype.lower():
    #                     logStatus.append('danger')
    #                 elif 'debug' in calltype.lower():
    #                     logStatus.append('success')
    #                 else:
    #                     logStatus.append(calltype.lower())
    #                 callback = i[34:].split('>', 1)[0].split(',')
    #                 logTable['Type'].append(calltype)
    #                 logTable['Function'].append(callback[0])
    #                 logTable['Line'].append(callback[1])
    #                 logTable['Message'].append(i[34:].split('>', 1)[1][4:].split('\n', 1)[0].replace('<', '').replace('>', ''))
    #     return self.genTable(logTable, typ=logStatus)


"""
.. py:class Settings()
Used to allow users to edit settings saved in _config.xml
"""
class Settings():

    """
    .. py:function __init__(self)
    Initialize the Settings object.
    """
    def __init__(self):
        self.log      = Logger('settings')
        self.utils    = Utils()
        self.binaries = Binaries()
        StartUp().startUp()

    """
    .. py:function settings(self)
    Build feedback for settings in Alfred.
    """
    def settings(self):
        feed = Feedback()
        feed.addItem('Download Directory', 'Edit where downloads are saved...', '1', '', '', '%s_edit.png' % self.binaries.icons)
        feed.addItem('Video Quality', 'Edit download quality of videos...', '2', '', '', '%s_edit.png' % self.binaries.icons)
        feed.addItem('Audio Quality', 'Edit conversion quality of audio...', '3', '', '', '%s_edit.png' % self.binaries.icons)
        if self.binaries.config.getProgressBar():
            feed.addItem('Progress Bar', 'Toggle progress bar OFF...', '4', '', '', '%s_check.png' % self.binaries.icons)
        else:
            feed.addItem('Progress Bar', 'Toggle progress bar ON...', '4', '', '', '%s_x.png' % self.binaries.icons)
        feed.addItem('Display Supported Domains', 'View the list of download supported doamins...', '5', '', '', '%s_entry.png' % self.binaries.icons)
        feed.addItem('Display About', 'View information about %s...' % PROGRAM, '6', '', '', '%s_entry.png' % self.binaries.icons)
        feed.addItem('Test Program State', 'Preferred way to debug errors...', '7', '', '', '%s_log.png' % self.binaries.icons)
        feed.addItem('View %s Log' % PROGRAM, 'If you run into errors you may be able to debug with the log...', '8', '', '', '%s_log.png' % self.binaries.icons)
        feed.addItem('%s Contributors' % PROGRAM, 'View people who have donated for %s...' % PROGRAM, '9', '', '', '%s_lux.png' % self.binaries.icons)
        return feed

    """
    .. py:function settingsDetermine(self, query)
    Determine which setting to engage from query passed.

    :param str query: Number to determine setting desired
    """
    def settingsDetermine(self, query):
        query = int(query)
        if query == 1:
            self.editDownloadDir()
        elif query == 2:
            self.editVideoOpt()
        elif query == 3:
            self.editAudioOpt()
        elif query == 4:
            self.toggleProgressBar()
        elif query == 5:
            self.displaySupported()
        elif query == 6:
            self.displayAbout()
        elif query == 7:
            self.testState()
        elif query == 8:
            self.displayLog()
        elif query == 9:
            self.displayContributors()
        else:
            pass

    """
    .. py:function editDownloadDir(self)
    Prompt the user to change the download directory.
    """
    def editDownloadDir(self):
        self.log.info('changing download dir')
        icon_file = '%s_lux.png' % self.binaries.icons
        if os.path.exists('%s_lux-dark.png' % self.binaries.icons):
            icon_file = '%s_lux-dark.png' % self.binaries.icons
        editProc = self.binaries.cocoa.fileselect(title = PROGRAM, label = 'Select where downloads should be saved by default...',
            select_only_directories = True, with_directory = self.binaries.config.getDownloadDir(), icon_file = icon_file)
        if len(editProc) > 0:
            self.binaries.config.editDownloadDir(editProc[0])
            self.log.info('change of download dir successful')
            self.binaries.notifier.notification(title = PROGRAM, subtitle = 'Changed Download Directory', sender = self.binaries.sender, 
                message = editProc[0], sound = 'Purr')

    """
    .. py:function editVideoOpt(self)
    Prompt the user to change the default video format.
    """
    def editVideoOpt(self):
        self.log.info('changing video opt')
        videoFormats = {'FLV 360p':'34', 'WebM 480p':'44', 'MP4 720p':'22', '[3D] WebM 720p':'102', '[3D] MP4 360p':'82', 
            '3GP 144p':'17', 'MP4 360p':'18', '[3D] MP4 240p':'83', '3GP 240p':'36', 'MP4 1080p':'37', 'WebM 720p':'45', 
            '[3D] WebM 360p':'100', 'WebM 1080p':'46', 'FLV 240p':'5', 'FLV 480p':'35', '[3D] MP4 520p':'85', '[3D] MP4 720p':'84', 
            'FLV 270p':'6', 'WebM 360p':'43'}
        icon_file = '%s_video.png' % self.binaries.icons
        if os.path.exists('%s_video-dark.png' % self.binaries.icons):
            icon_file = '%s_video-dark.png' % self.binaries.icons
        editProc = self.binaries.cocoa.dropdown(title = PROGRAM, label = 'Select your desired video download format...',
            button1 = 'Select', items = videoFormats.keys(), height = '130', string_output = True, button2 = 'Revert to Default',
            icon_file = icon_file)
        if 'select' in editProc[0].lower():
            self.binaries.config.editVideoOpt(videoFormats[editProc[1]])
            self.log.info('change of video opt successful')
            self.binaries.notifier.notification(title = PROGRAM, subtitle = 'Changed Preferred Video Format', sender = self.binaries.sender,
                message = editProc[1], sound = 'Purr')
        else:
            self.binaries.config.editVideoOpt('', default = True)
            self.log.info('reset video opt to default')
            self.binaries.notifier.notification(title = PROGRAM, subtitle = 'Changed Preferred Video Format', sender = self.binaries.sender,
                message = 'BEST POSSIBLE', sound = 'Purr')

    """
    .. py:function editAudioOpt(self)
    Prompt the user to change the default audio format.
    """
    def editAudioOpt(self):
        self.log.info('changing audio opt')
        audioFormats = ['.mp3', '.wav', '.ogg', '.m4a', '.wma']
        icon_file = '%s_audio.png' % self.binaries.icons
        if os.path.exists('%s_audio-dark.png' % self.binaries.icons):
            icon_file = '%s_audio-dark.png' % self.binaries.icons
        editProc = self.binaries.cocoa.dropdown(title = PROGRAM, label = 'Select your desired audio download format...',
            button1 = 'Select', items = audioFormats, height = '130', string_output = True, button2 = 'Revert to Default',
            icon_file = icon_file)
        if 'select' in editProc[0].lower():
            self.binaries.config.editAudioOpt(editProc[1])
            self.log.info('change of audio opt successful')
            self.binaries.notifier.notification(title = PROGRAM, subtitle = 'Changed Preferred Audio Format', sender = self.binaries.sender,
                message = editProc[1], sound = 'Purr')
        else:
            self.binaries.config.editAudioOpt('', default = True)
            self.log.info('reset audio opt to default')
            self.binaries.notifier.notification(title = PROGRAM, subtitle = 'Changed Preferred Audio Format', sender = self.binaries.sender,
                message = '%s (Default)' % self.binaries.config.getAudioOpt(), sound = 'Purr')

    """
    .. py:function displayHistory(self)
    Return feedback of recorded history to Alfred.
    """
    def displayHistory(self):
        feed = Feedback()
        if len(self.binaries.config.getHistory()) > 0:
            feed.addItem('Clear History', 'Erase all history entries...', '1', '', '', '%s_erase.png' % self.binaries.icons)
            for i in self.binaries.config.getHistory():
                feed.addItem(i['title'], i['url'], i['url'], '0', '', '%s_entry.png' % self.binaries.icons)
        if len(feed.feedback) <= 0:
            feed.addItem('No History', 'You have no history entries...', '0', '', '', '%s_x.png' % self.binaries.icons)
        return feed

    """
    .. py:function clearHistory(self, query):
    Clear all history if query is greater than zero.

    :param str query: Number 0 or >0
    """
    def clearHistory(self, query):
        if self.utils.isInt(query) and int(query) > 0:
            self.binaries.config.clearHistory()
            self.binaries.notifier.notification(title = PROGRAM, subtitle = 'Cleared History', sender = self.binaries.sender,
                message = 'Removed all history entries', sound = 'Purr')
        else:
            webbrowser.open(query)

    """
    .. py:function toggleProgressBar(self)
    Toggle the progress bar between True and False.
    """
    def toggleProgressBar(self):
        self.log.info('toggling progress bar to (%s)' % str(not self.binaries.config.getProgressBar()))
        self.binaries.config.toggleProgressBar()
        self.binaries.notifier.notification(title = PROGRAM, subtitle = 'Toggled Progress Bar', sender = self.binaries.sender,
            message = 'Progress Bar is now %s' % ('ON' if self.binaries.config.getProgressBar() else 'OFF'), sound = 'Purr')

    """
    .. py:function displayAbout(self)
    Display the program's about text in a cocoa textbox.
    """
    def displayAbout(self):
        self.log.info('displaying about markdown')
        self.binaries.notifier.notification(
            title = PROGRAM, 
            subtitle = 'Displaying About', 
            message = 'Building markdown template...', 
            sender = self.binaries.sender, 
            sound = '')
        markdown = GenMD(theme = 'yeti')
        markdown.displayMD(markdown.customGenAbout())

    """
    .. py:function displaySupported(self)
    Display youtube_dl's latest supported domains in a cocoa textbox.
    """
    def displaySupported(self):
        self.log.info('displaying supported domains MD')
        self.binaries.notifier.notification(
            title = PROGRAM, 
            subtitle = 'Generating Supported Domains', 
            sender = self.binaries.sender,
            message = 'Building markdown template...', 
            sound = '')
        markdown = GenMD(theme = 'yeti')
        markdown.displayMD(markdown.customGenSupportedDomains())

    """
    .. py:function resetToDefaults(self)
    Prompt the user to reset all settings of Luxinate's config to default.
    """
    def resetToDefaults(self):
        self.log.info('resetting config\'s current state to defaults')
        editProc = self.binaries.cocoa.msgbox(title = PROGRAM, 
            text = 'Resetting %s to Default Settings...' % PROGRAM, 
            button1 = 'Yes', button2 = 'No', 
            informative_text = 'Are you sure you want to continue?', 
            icon_file = '%sicon.png' % self.binaries.workflow)
        if int(editProc[0]) == 1:
            self.binaries.config.editDownloadDir('', default = True)
            self.binaries.config.editVideoOpt('', default = True)
            self.binaries.config.editAudioOpt('', default = True)
            self.binaries.config.editMultiPid('', default = True)
            while self.binaries.config.getProgressBar():
                self.binaries.config.toggleProgressBar()
            self.binaries.config.clearHistory()
            self.log.info('reset successful')
            self.binaries.notifier.notification(title = PROGRAM, 
                subtitle = '%s Reset to Defaults' % PROGRAM, 
                sender = self.binaries.sender,
                message = 'Reset Successful', 
                sound = 'Purr')

    """
    .. py:function displayLog(self)
    Open the LOG inside the Console application.
    """
    def displayLog(self):
        self.log.info('opening log at (%s) to Console.app' % LOG)
        self.utils.runProcess('open -a "Console.app" %s' % self.utils.formatConsole(LOG))

    """
    .. py:function testState(self)
    Test Luxinate's current state.
    """
    def testState(self):
        self.log.info('testing program state')
        self.binaries.notifier.notification(title = PROGRAM, subtitle = 'Testing %s\'s State' % PROGRAM, sender = self.binaries.sender,
            message = 'Buidling markdown template...', sound = '')
        markdown = GenMD(theme = 'yeti')
        markdown.displayMD(markdown.customGenTest())

    """
    .. py:function displayContributors(self)
    Display contributors to Luxinate.
    """
    def displayContributors(self):
        self.log.info('displaying contributors MD')
        self.binaries.notifier.notification(title = PROGRAM, subtitle = 'Retrieving Contributors', sender = self.binaries.sender,
            message = 'Building markdown template...', sound = '')
        markdown = GenMD(theme = 'yeti')
        markdown.displayMD('%s%s' % (markdown.customGenDonate(), markdown.customGenContributors()))


"""
.. py:class StartUp()
Used to check and initialize the program before run.
"""
class StartUp():

    """
    .. py:function __init__(self)
    Initialize the StartUp object.
    """
    def __init__(self):
        self.log      = Logger('StartUp')
        self.utils    = Utils()
        self.binaries = Binaries() 

    """
    .. py:function startUp(self)
    Used to check that the program's environment is in order before running.
    """
    def startUp(self):
        if not os.path.exists(self.binaries.resources):
            os.system('mkdir %s' % self.utils.formatConsole(self.binaries.resources))
        if not os.path.exists(self.binaries.config.getDownloadDir()):
            os.system('mkdir %s' % self.utils.formatConsole(self.binaries.config.getDownloadDir()))
        if not os.path.exists(self.binaries.pkgmanager):
            self.log.critical('cannot find package manager at (%s)' % self.binaries.pkgmanager)
            if not os.path.exists(self.binaries.cocoa.cocoa):
                if not os.path.exists(self.binaries.notifier.notifier):
                    self.utils.runOsascript('tell application "Alfred 2" to search "Error: missing package manager"')
                else:
                    self.binaries.notifier.notification(title = PROGRAM, subtitle = ':: WARNING ::',
                        message = 'Missing package manager...', sender = self.sender, group = 'lux.warning')
            else:
                self.binaries.cocoa.msgbox(title = PROGRAM, text = ':: WARNING ::',
                    informative_text = 'Missing package manager...', button1 = 'Ok')
            sys.exit(0)
        currentPython = '%s.%s' % (sys.version_info[0], sys.version_info[1])
        requiredPython = self.binaries.config.getRoot().find('.//python').text
        if float(currentPython) != float(requiredPython):
            self.log.critical('invalid version of python (%s)' % currentPython)
            if not os.path.exists(self.binaries.cocoa.cocoa):
                if not os.path.exists(self.binaries.notifier.notfier):
                    self.utils.runOsascript('tell application "Alfred 2" to search "Error: invalid Python version %s"' % currentPython)
                else:
                    self.binaries.notifier.notification(title = PROGRAM, subtitle = ':: WARNING ::',
                        message = 'Invalid Python Version (%s)...' % currentPython, sender = self.sender, group = 'lux.warning')
            else:
                self.binaries.cocoa.msgbox(title = PROGRAM, text = ':: WARNING ::',
                    informative_text = 'Invalid Python Version (%s)...' % currentPython, button1 = 'Ok')
            sys.exit(0)
        missingDependencies = []
        for i in self.binaries.dependencies:
            if not os.path.exists(i['loci']):
                self.log.warning('cannot find dependency "%s" at (%s)' % (i['title'], i['loci']))
                missingDependencies.append(i)
        if len(missingDependencies) > 0:
            for i in missingDependencies:
                self.utils.runProcess('%s -i %s -o %s' % (self.utils.formatConsole(self.binaries.pkgmanager), 
                    i['title'], self.utils.formatConsole(i['dest'])))
        self.utils.runProcess('%s -dark -dark -light -light --suppress' % self.utils.formatConsole(self.binaries.glyphmgr))
        if not self.binaries.config.getAbout():
            self.binaries.config.toggleAbout()
            Settings().displayAbout()
