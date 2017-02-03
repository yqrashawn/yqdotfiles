#!/usr/bin/python
# encoding: utf-8

from cal import Cal
from datetime import date
from base import Base
from util import get_from_dict
from util import DEFAULT_SETTINGS
import sys
import re


class Main(Base):

    config_option = u"config"
    config_desc = u"Open config file"

    def init_settings(self):
        self.minus = get_from_dict(self.wf.settings, 'minus', DEFAULT_SETTINGS['minus'])
        self.plus = get_from_dict(self.wf.settings, 'plus', DEFAULT_SETTINGS['plus'])
        self.first_day = get_from_dict(self.wf.settings, 'first_day', DEFAULT_SETTINGS['first_day'])
        get_from_dict(self.wf.settings, 'weekdays', DEFAULT_SETTINGS['weekdays'])
        get_from_dict(self.wf.settings, "month", DEFAULT_SETTINGS['month'])
        get_from_dict(self.wf.settings, "width", DEFAULT_SETTINGS['width'])
        get_from_dict(self.wf.settings, "highlight_today", DEFAULT_SETTINGS['highlight_today'])

    def main(self, wf):
        self.init_settings()
        self.pattern = re.compile('^(%s|%s)*$' % (self.minus, self.plus))

        cal = Cal(wf.settings, wf.alfred_env['theme'], wf.alfred_env['preferences'])
        try:
            year, month = self.handle_arg()
        except InvalidArgumentError as e:
            self.wf.add_item(e.message)
            self.wf.send_feedback()
            return

        if year == -1 and month == -1:
            return
        texts = cal.get_weeks_text(year, month, self.first_day)
        for index, text in enumerate(texts):
            if index == 0:
                self.wf.add_item(text)
            elif index < 2:
                self.wf.add_item(text, icon="image/blank.png")
            else:
                arg = "%d %d %s" % (year, month, text.strip().split()[0])
                self.wf.add_item(text, icon="image/biank.png", arg=arg, valid=True)
        self.wf.send_feedback()

    def handle_arg(self):

        if self.config_option.startswith(self.args) and len(self.args) > 0:
            self.wf.add_item(self.config_desc, valid=True, arg=self.wf.settings_path, autocomplete=self.config_option)
            self.wf.send_feedback()
            return -1, -1

        argv = self.args.split()
        today = date.today()

        month, get_month = self.to_month(self.get_item(argv, 0), today.month)
        year, get_year = self.to_year(self.get_item(argv, 1), today.year)

        # shift the used argument
        if get_month:
            argv = argv[1:]
        if get_year:
            argv = argv[1:]

        delta_str = self.get_item(argv, 0, "")
        delta = delta_str.count(self.plus) - delta_str.count(self.minus)

        year, month = self.change_month(year, month, delta)

        return year, month

    def get_item(self, arr, index, default_value=None):
        try:
            return arr[index]
        except IndexError:
            return default_value

    def to_month(self, month, default_month):
        try:
            ret = int(month)
            if ret >= 1000:
                raise InvalidArgumentError("Please enter month before year")
            elif ret <= 0 or ret > 12:
                raise InvalidArgumentError("month must be in 1..12")
            return ret, True
        except TypeError:
            return default_month, False
        except InvalidArgumentError:
            raise
        except:
            month = month.lower()
            if month[-1] == '.' and len(month) > 1:
                month = month[:-1]
            for i, cal_month in enumerate(self.wf.settings['month'].split()):
                if cal_month.lower().startswith(month):
                    return i + 1, True

            if not self.is_move(month):
                raise InvalidArgumentError('Please enter valid month argument.')
            else:
                return default_month, False

    def to_year(self, year, default_year):
        try:
            return int(year), True
        except TypeError:
            return default_year, False
        except:
            if not self.is_move(year):
                raise InvalidArgumentError('Please enter valid year argument.')
            else:
                return default_year, False
    
    def is_move(self, str):
        return self.pattern.match(str)

    def change_month(self, year, month, delta):
        month += delta
        ret_month = month % 12
        ret_year = year + month / 12
        if ret_month == 0:
            ret_month = 12
        if month % 12 == 0:
            ret_year -= 1
        return ret_year, ret_month


class InvalidArgumentError(Exception):
    pass

if __name__ == "__main__":
    main = Main(" ".join(sys.argv[1:]))
    main.execute()
