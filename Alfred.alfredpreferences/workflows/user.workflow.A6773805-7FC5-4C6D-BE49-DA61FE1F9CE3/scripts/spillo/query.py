import argparse
import sys
import unicodedata


class Query(object):
    @staticmethod
    def parse_query(query_string):
        """Factory method that parses a query string and returns a query
        subclass instance."""
        parser = _QueryParser()

        parser.add_argument('value', nargs='*')
        parser.add_argument('-n', '--name', nargs='+', dest='name')
        parser.add_argument('-u', '--url', nargs='+', dest='url')
        parser.add_argument('-d', '--desc', nargs='+', dest='desc')
        parser.add_argument('-t', '--tags', nargs='+', dest='tags')
        parser.add_argument('-un', '--unread', nargs='?', dest='unread')
        parser.add_argument('-pu', '--public', nargs='?', dest='public')

        # make sure that the unicode query string is decoded
        try:
            query_string = unicodedata.normalize('NFC', query_string.decode('utf-8'))
        except UnicodeDecodeError:
            pass

        # try to parse the arguments, if an exception is thrown it's because some args
        # are incomplete and we shouldn't return any result until they are
        try:
            args = vars(parser.parse_args(query_string.split()))
        except:
            raise QueryException()

        if args['value']:
            return QueryGlobal(_format_string_arg(args, 'value'))

        name = _format_string_arg(args, 'name')
        url = _format_string_arg(args, 'url')
        desc = _format_string_arg(args, 'desc')
        tags = args['tags']
        unread = _format_boolean_arg(args, 'unread')
        public = _format_boolean_arg(args, 'public')

        return QuerySpecific(name, url, desc, tags, unread, public)


def _format_string_arg(args, key):
    return ' '.join(args[key]) if args[key] else None


def _format_boolean_arg(args, key):
    value = args[key]
    if value:
        if isinstance(value, basestring):
            value = value.lower()
            if value == 'yes' or value == 'true':
                return 1
            if value == 'no' or value == 'false':
                return 0
        try:
            return 0 if int(value) == 0 else 1
        except (ValueError, TypeError):
            pass
    return None


class QueryGlobal(Query):
    _value = None

    def __init__(self, value):
        self._value = value

    @property
    def value(self):
        return self._value


class QuerySpecific(Query):
    _name = None
    _url = None
    _desc = None
    _tags = None
    _unread = None
    _public = None

    def __init__(self, name, url, desc, tags, unread, public):
        self._name = name
        self._url = url
        self._desc = desc
        self._tags = tags
        self._unread = unread
        self._public = public

    @property
    def name(self):
        return self._name

    @property
    def url(self):
        return self._url

    @property
    def desc(self):
        return self._desc

    @property
    def tags(self):
        return self._tags

    @property
    def unread(self):
        return self._unread

    @property
    def public(self):
        return self._public


class QueryException(Exception):
    pass


class _QueryParser(argparse.ArgumentParser):
    def _get_action_from_name(self, name):
        container = self._actions
        if name is None:
            return None
        for action in container:
            if '/'.join(action.option_strings) == name:
                return action
            elif action.metavar == name:
                return action
            elif action.dest == name:
                return action

    def error(self, message):
        exc = sys.exc_info()[1]
        if exc:
            exc.argument = self._get_action_from_name(exc.argument_name)
            raise exc
        super(_QueryParser, self).error(message)
