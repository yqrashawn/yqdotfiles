import sys
import json
from workflow import Workflow
from os.path import expanduser


ICON_DEFAULT = 'icon.png'
CONFIG_PATH = '.config/karabiner/karabiner.json'
home = expanduser("~")


def main(wf):
    with open('{}/{}'.format(home, CONFIG_PATH)) as json_data:
        config = json.load(json_data)
        for profile in config['profiles']:
            wf.add_item(
                profile['name'], 'Keyboard Preset Profile',
                arg=profile['name'], valid=True, icon=ICON_DEFAULT)
    wf.send_feedback()

if __name__ == u"__main__":
    wf = Workflow()
    sys.exit(wf.run(main))
