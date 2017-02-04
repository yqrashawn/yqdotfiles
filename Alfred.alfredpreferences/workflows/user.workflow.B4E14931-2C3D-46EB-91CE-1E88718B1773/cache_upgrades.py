"""
Scripts to upgrade the cache, if necessary.
"""


def upgrade_1 (cache):
  if '$q' in cache:
    del cache['$q']

upgrades = [upgrade_1]
