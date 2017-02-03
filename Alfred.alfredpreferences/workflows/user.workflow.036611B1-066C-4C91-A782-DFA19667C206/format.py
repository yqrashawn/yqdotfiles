#!/usr/bin/python
# encoding: utf-8

import tkFont
import plistlib
import Tkinter as tk

plist = "/preferences/appearance/prefs.plist"
theme = "theme.plist"
DEFAULT_FONT = "Helvetica Neue"
DEFAULT_SIZE = 16


class Format(object):
    def __init__(self, key, path):
        t = tk.Tk()
        self.font = self._load_font(key, path)

    def _load_font(self, key, path):
        try:
            font, size = self._load_plist(key, path + plist)
        except:
            try:
                font, size = self._load_plist(key, theme)
            except:
                font, size = DEFAULT_FONT, DEFAULT_SIZE
        return tkFont.Font(family=font, size=size)
    
    def _load_plist(self, key, path):
        pref = plistlib.readPlist(path)
        font = pref['themes'][key]['resultTextFont']
        size = pref['themes'][key]['resultTextFontSize'] * 4 + 8
        return font, size

    def format(self, weeks, title):
        pos = [self.font.measure(title[:title.find(day)]) for day in title.split()]
        space_width = self.font.measure(u' ')
        str_list = []
        for week in weeks:
            str = ""
            for i, day in enumerate(week):
                number_of_spaces = (pos[i] - self.font.measure(str)) / space_width
                if (pos[i] - self.font.measure(str)) % space_width > space_width / 2:
                    number_of_spaces += 1
                str += u' ' * number_of_spaces
                str += day
            str_list.append(str)
        return str_list


if __name__ == "__main__":
    key = "alfred.theme.custom.A1911D25-FB72-4E1C-9180-7A8A71DB327F"
    path = "/Users/owen/Library/Application Support/Alfred 2/Alfred.alfredpreferences"
    f = Format(key, path)
    from cal import Cal
    c = Cal({}, key, path)
    print f.format(c.get_cal(c.get_weeks(2015, 1)), c.week_text(6))
