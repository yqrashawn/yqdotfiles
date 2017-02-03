#!/usr/bin/python
# encoding: utf-8

import calendar
from datetime import date
from format import Format


class Cal(object):
    
    def __init__(self, settings, key, path):
        self.weekdays_name = settings["weekdays"].split()
        self.month_name = settings["month"].split()
        self.width = int(settings["width"])
        self.highlight_today = settings["highlight_today"]

        self.key = key
        self.path = path
    
    def get_weeks(self, year, month, first_weekday):
        cal = calendar.Calendar(first_weekday)
        return list(cal.itermonthdates(year, month))

    def get_weeks_text(self, year, month, first_weekday):
        texts = []
        texts.append(self.month_text(year, month))
        texts.append(self.week_text(first_weekday))
        format = Format(self.key, self.path)
        texts += format.format(self.get_cal(self.get_weeks(year, month, first_weekday)), texts[-1])
        return self.center(texts)

    def month_text(self, year, month):
        return self.month_name[month - 1] + "  " + str(year)
    
    def week_text(self, first_weekday):
        return ''.join([weekday.center(self.width) for weekday in self.new_weekdays_name(first_weekday)])
            
    def new_weekdays_name(self, first_weekday):
        return self.weekdays_name[first_weekday:] + self.weekdays_name[:first_weekday]

    def get_cal(self, days):
        current_month = days[len(days) / 2].month
        weeks = []
        week = []
        for day in days:
            if day.month != current_month:
                week.append('')
            else:
                week.append(self.str_day(day))
            if len(week) == 7:
                weeks.append(week)
                week = []
        return weeks

    def str_day(self, day):
        if day == date.today() and self.highlight_today:
            return unichr(0x2605)
        else:
            return unicode(day.day).zfill(2)

    def center(self, texts):
        texts[0] = texts[0].center(len(texts[1]))
        return texts

    def add_pre_space(self, count):
        return u" " * (count * self.width * 2 / 3)


if __name__ == "__main__":
    key = "alfred.theme.custom.A1911D25-FB72-4E1C-9180-7A8A71DB327F"
    path = "/Users/owen/Library/Application Support/Alfred 2/Alfred.alfredpreferences"
    c = Cal({}, key, path)
    for line in c.get_weeks_text(2014, 11, 6):
        print line
