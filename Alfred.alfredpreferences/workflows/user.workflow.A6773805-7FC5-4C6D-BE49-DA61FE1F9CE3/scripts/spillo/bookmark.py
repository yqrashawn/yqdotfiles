class Bookmark(object):
    _url = None
    _title = None
    _identifier = None
    _date = None

    def __init__(self, url, title, identifier, date):
        self._url = url
        self._title = title
        self._identifier = identifier
        self._date = date

    @property
    def url(self):
        return self._url

    @url.setter
    def url(self, url):
        self._url = url

    @property
    def title(self):
        return self._title

    @title.setter
    def title(self, title):
        self._title = title

    @property
    def identifier(self):
        return self._identifier

    @identifier.setter
    def identifier(self, identifier):
        self._identifier = identifier

    @property
    def date(self):
        return self._date

    @date.setter
    def date(self, date):
        self._date = date
