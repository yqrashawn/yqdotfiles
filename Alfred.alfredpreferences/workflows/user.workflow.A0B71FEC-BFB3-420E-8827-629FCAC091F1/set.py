import sys
import json
from os.path import expanduser
from collections import OrderedDict

CHOSEN_PROFILE = sys.argv[1]
CONFIG_PATH = '.config/karabiner/karabiner.json'


home = expanduser("~")
config = {}

with open('{}/{}'.format(home, CONFIG_PATH)) as conf_file:
    config = json.load(conf_file, object_pairs_hook=OrderedDict)
    for profile in config['profiles']:
        profile['selected'] = profile['name'] == CHOSEN_PROFILE

with open('{}/{}'.format(home, CONFIG_PATH), 'w') as conf_file:
    conf_file.write(json.dumps(config, indent=4, separators=(',', ': ')))
