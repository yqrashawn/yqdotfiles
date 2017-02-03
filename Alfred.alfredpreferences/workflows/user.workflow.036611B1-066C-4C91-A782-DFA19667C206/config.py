#!/usr/bin/python
# encoding: utf-8

from util import get_from_dict
from base import Base
import json


def show(func):
    def wrapper(self, *args, **kwargs):
        func(self, *args, **kwargs)
        self.wf.send_feedback()
    return wrapper


class Config(Base):
    
    separator = ' '
    open_config_file = "open_config_file"
    
    def main(self, wf):
        self._handle_arg()
        self._load_json()
        config = self._filter_config_item()

        if len(config) == 1 and config[0]['keyword'] == self.option:
            if 'list' in config[0]:
                self._show_sub_list(config[0]['list'], config[0], self.value)
                return
            elif 'action' in config[0]:
                self._show_action_item(config[0])
                return
            elif self.value != "":
                self._show_item(config[0], self.value)
                return

        if len(self.config) != 0:
            self.config = config
        
        self._show_list()

    def set_value(self):
        from workflow import Workflow
        self.wf = Workflow()
        if self.args == self.open_config_file:
            self._open_file(self.wf.settings_path)
        else:
            self._handle_arg()
            old_value = self.wf.settings[self.option]
            self.wf.settings[self.option] = type(old_value)(self.value)

    def load_default(self, key):
        self._load_json()
        for item in self.config:
            if key == item['name']:
                return item['default']
        return None
    
    def _open_file(self, file):
        import subprocess
        subprocess.call(['open', file])

    def _handle_arg(self):
        self.option, _, self.value = self.args.partition(self.separator)
        self.option = self.option.strip()
        self.value = self.value.strip()

    def _load_json(self, file="config.json"):
        if not hasattr(self, 'config') or not self.config:
            with open(file) as fp:
                self.config = json.load(fp)

    def _filter_config_item(self):
        return [item for item in self.config if item['keyword'].startswith(self.option)]
    
    @show
    def _show_list(self):
        for item in self.config:
            title, subtitle = self._get_text(item)
            self.wf.add_item(title, subtitle=subtitle, autocomplete=item['keyword'] + " ")

    @show
    def _show_item(self, item, new_value):
        try:
            self._check_valid(item, new_value)
            title, subtitle = self._get_text(item)
            subtitle += ", set to: " + new_value
            self.wf.add_item(title,
                             subtitle=subtitle,
                             valid=True,
                             arg=item['name'] + Config.separator + new_value)
        except InvalidInputError as e:
            self.wf.add_item(e.message)
            return

    @show
    def _show_sub_list(self, sub_list, item, value):
        current_value = get_from_dict(self.wf.settings, item['name'], item['default'])
        for sub_item, sub_value in sub_list:
            if value.lower() in sub_item.lower():
                title = sub_item
                if sub_value == current_value:
                    title += " (selected)"
                self.wf.add_item(title,
                                 valid=True,
                                 arg=item['name'] + Config.separator + str(sub_value),
                                 autocomplete=item['keyword'] + ' ' + sub_item.lower())

    @show
    def _show_action_item(self, item):
        self.wf.add_item(item['description'],
                         valid=True,
                         arg=self.open_config_file)
                         
    def _check_valid(self, item, new_value):
        return getattr(self, '_check_' + item['type'])(item, new_value)

    def _get_text(self, item):
        title = item['description']
        if 'name' in item and 'default' in item:
            current_value = get_from_dict(self.wf.settings, item['name'], item['default'])
            if 'list' in item:
                current_value = next((i for i in item['list'] if i[1] == current_value))[0]
            subtitle = u"Current: %s" % (current_value)
        else:
            subtitle = ""
        return title, subtitle

    def _check_int(self, item, new_value):
        not_int = "Please enter an integer"
        too_small = "Value must be larger than %s"
        too_large = "Value must be smaller than %s"
        out_of_range = "Value must be between %s and %s"

        try:
            value = int(new_value)
            if 'min' in item and 'max' in item and (not item['min'] <= value <= item['max']):
                raise InvalidInputError(out_of_range % (item['min'], item['max']))
            elif 'min' in item and value < item['min']:
                raise InvalidInputError(too_small % (item['min']))
            elif 'max' in item and value > item['max']:
                raise InvalidInputError(too_large % (item['max']))
        except ValueError:
            raise InvalidInputError(not_int)
        
    def _check_str(self, item, new_value):
        return True

    def _check_list(self, item):
        return True


class InvalidInputError(Exception):
    pass

if __name__ == "__main__":
    import sys
    c = Config(' '.join(sys.argv[1:]))
    c.execute()
