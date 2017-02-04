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

from cache_upgrades import upgrades
from persistent_dict import PersistentDict

cache_filename = 'npms.cache'
cache_expiry = int(getenv('NPMS_CACHE_EXPIRATION', 86400))
url = 'https://api.npms.io/v2/search?q=%s'


def search(term):
  """
  certain Mac OS versions ship with a broken openssl which prevents
  fetching over https.  search for "SSLV3_ALERT_HANDSHAKE_FAILURE" for
  details.  so use curl here, I guess.
  """

  try:
    # print >> sys.stderr, ''.join(['/usr/bin/curl', '-L', url % quote_plus(term)])
    response = check_output(['/usr/bin/curl', '-L', url % quote_plus(term)])
  except CalledProcessError, e:
    return dict(error='cURL failure', reason=str(e))
  else:
    # print >> sys.stderr, response
    try:
      return json.loads(response)
    except ValueError, e:
      return dict(error='Invalid server response', reason=str(e))


def pruneCache(cache):
  for term in [key for (key, value) in cache.iteritems() if
               type(value) == 'dict' and
                 (not value['timestamp'] or time() - value[
                 'timestamp'] > cache_expiry)]:
    del cache[term]
  try:
    cache.sync()
  except IOError:
    pass
  return cache


def upgradeCache():
  cache = PersistentDict(cache_filename)
  if '__VERSION__' not in cache:
    version = cache['__VERSION__'] = 0
  else:
    version = cache['__VERSION__']

  for upgrade_func in upgrades[version:]:
    try:
      upgrade_func(cache)
      cache['__VERSION__'] += 1
    except:
      break

  try:
    cache.sync()
  except IOError:
    pass

  return cache


def npms():
  items = []
  try:
    term = sys.argv[1]
  except IndexError:
    return dict(items=items)
  else:
    cache = upgradeCache()
    pruneCache(cache)

    if term in cache:
      items = cache[term]['items']
    else:
      data = search(term)

      if 'error' in data:
        return dict(items=[
          dict(title='Error: %s' % data['error'], subtitle=data['reason'],
               valid=False)])

      if 'results' in data and data['results']:
        for result in data['results']:
          module = result['package']
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
        try:
          cache.sync()
        except IOError:
          pass
      else:
        items.append(dict(
          title='No matches!  Try search on npms.io?',
          subtitle='(Repeat search for "%s" on npms.io)' % term,
          arg='https://npms.io/search?q=%s' % term
        ))
        return dict(items=items)

    items.append(dict(title='Show more results on npms.io...',
                      subtitle='(Repeat search for "%s" on npms.io)' % term,
                      arg='https://npms.io/search?q=%s' % term))
    return dict(items=items)


print json.dumps(npms())
