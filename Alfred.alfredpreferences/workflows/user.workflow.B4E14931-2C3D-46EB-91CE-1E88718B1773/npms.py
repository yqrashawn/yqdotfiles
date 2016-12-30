#!/usr/bin/python

"""
alfred-npms - https://github.com/boneskull/alred-npms

Alfred workflow for npms.io; searches npms.io for an npm package

Copyright 2016 Christopher Hiller.  Licensed MIT
"""

import json
import sys
from os import getenv
from subprocess import CalledProcessError, check_output
from time import time
from urllib import quote_plus

from persistent_dict import PersistentDict

cache_filename = 'npms.cache'
cache_expiry = int(getenv('NPMS_CACHE_EXPIRATION', 86400))
url = 'https://api.npms.io/search?term=%s'


def search(term):
  """
  certain Mac OS versions ship with a broken openssl which prevents
  fetching over https.  search for "SSLV3_ALERT_HANDSHAKE_FAILURE" for
  details.  so use curl here, I guess.
  """

  try:
    response = check_output(['/usr/bin/curl', '-L', url % quote_plus(term)])
  except CalledProcessError, e:
    return dict(error='cURL failure', reason=str(e))
  else:
    try:
      return json.loads(response)
    except ValueError, e:
      return dict(error='Invalid server response', reason=str(e))


def npms():
  items = []
  try:
    term = sys.argv[1]
  except IndexError:
    return dict(items=items)
  else:
    with PersistentDict(cache_filename) as cache:
      if term in cache and time() - cache[term]['timestamp'] < cache_expiry:
        return dict(items=cache[term]['items'])

      data = search(term)

      if 'error' in data:
        return dict(items=[dict(title='Error: %s' % data['error'], subtitle=data['reason'], valid=False)])

      if 'results' in data and data['results']:
        for result in data['results']:
          module = result['module']
          items.append(dict(
              title='%s @ %s' % (module['name'], module['version']),
              subtitle=module.get('description', '(no description)'),
              arg=module['links']['npm'],
              mods=dict(
                  alt=dict(
                    arg=module['links'].get('homepage', 'repository'),
                    subtitle='Open project homepage'),
                  cmd=dict(
                      arg=module['links'].get('repository', 'npm'),
                      subtitle='Open project repository')),
              text=dict(
                  copy=module['links']['npm'],
                  largetype=module['name'])))
        cache[term] = dict(items=items, timestamp=int(time()))
      else:
        items.append(dict(
            title='No matches!',
            subtitle='(Repeat search on site)',
            arg='https://npms.io/search?term=%s' % term
        ))

      return dict(items=items)


print json.dumps(npms())
