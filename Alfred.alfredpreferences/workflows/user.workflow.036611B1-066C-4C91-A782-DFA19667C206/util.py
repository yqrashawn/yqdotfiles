#!/usr/bin/python
# encoding: utf-8

DEFAULT_SETTINGS = {
    "first_day": 6,
    "highlight_today": True,
    "minus": u"<",
    "month": u"January February March April May June July August September October November December",
    "plus": u">",
    "software": u"calendar",
    "weekdays": u"Mo Tu We Th Fr Sa Su",
    "width": 10
}


def get_from_dict(dict, key, default_value):
    try:
        return dict[key]
    except KeyError:
        dict[key] = default_value
        return default_value
