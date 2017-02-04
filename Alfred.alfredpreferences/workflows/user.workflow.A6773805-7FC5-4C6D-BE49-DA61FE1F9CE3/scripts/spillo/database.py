import os
import sqlite3

from query import (
    QueryGlobal,
    QuerySpecific,
)
from bookmark import Bookmark

from Foundation import (
    NSLibraryDirectory,
    NSSearchPathForDirectoriesInDomains,
    NSUserDomainMask,
)


class Database(object):
    def __init__(self, account_identifier):
        self.connection = sqlite3.connect(self._retrieve_database_path(account_identifier))

    def query(self, query):
        try:
            cursor = self.connection.cursor()
            if isinstance(query, QueryGlobal):
                return self._query_global(cursor, query)
            elif isinstance(query, QuerySpecific):
                return self._query_specific(cursor, query)
            else:
                raise DatabaseException('Unexpected query type')
        except sqlite3.OperationalError:
            raise DatabaseException('There was an unknown error while querying the database')

    # the name or url or desc is like the search term or the tag is exactly the search term
    GLOBAL_QUERY = 'SELECT ZTITLE, ZURL, ZIDENTIFIER, ZDATE FROM ZPINBOARDPOST WHERE ZDELETING=0 AND \
                    ZTITLE LIKE ? OR ZURL LIKE ? OR ZDESC LIKE ? OR Z_PK IN \
                        (SELECT Z_2POSTS FROM Z_2TAGS WHERE Z_3TAGS == \
                            (SELECT Z_PK FROM ZPINBOARDTAG WHERE ZTITLE == ? COLLATE NOCASE)) COLLATE NOCASE'

    def _query_global(self, cursor, query):
        # construct a search for each word in the query and intersect the queries
        queries = []
        params = []
        for word in query.value.split():
            queries.append(Database.GLOBAL_QUERY)
            params.append('%' + word + '%')  # name
            params.append('%' + word + '%')  # url
            params.append('%' + word + '%')  # desc
            params.append(word)  # tag

        if not queries:
            return None

        sql = ' INTERSECT '.join(queries) + ' ORDER BY ZDATE DESC'
        cursor.execute(sql, params)
        return self._generate_bookmarks(cursor)

    # the name is like the search term
    NAME_QUERY = 'SELECT ZTITLE, ZURL, ZIDENTIFIER, ZDATE FROM ZPINBOARDPOST WHERE ZDELETING=0 AND ZTITLE LIKE ? COLLATE NOCASE'
    # the url is like the search term
    URL_QUERY = 'SELECT ZTITLE, ZURL, ZIDENTIFIER, ZDATE FROM ZPINBOARDPOST WHERE ZDELETING=0 AND ZURL LIKE ? COLLATE NOCASE'
    # the desc is like the search term
    DESC_QUERY = 'SELECT ZTITLE, ZURL, ZIDENTIFIER, ZDATE FROM ZPINBOARDPOST WHERE ZDELETING=0 AND ZDESC LIKE ? COLLATE NOCASE'
    # the tag is exactly the search term
    TAG_QUERY = 'SELECT ZTITLE, ZURL, ZIDENTIFIER, ZDATE FROM ZPINBOARDPOST WHERE ZDELETING=0 AND Z_PK IN \
                     (SELECT Z_2POSTS FROM Z_2TAGS WHERE Z_3TAGS == \
                         (SELECT Z_PK FROM ZPINBOARDTAG WHERE ZTITLE == ? COLLATE NOCASE))'
    # the unread status is 1 or 0
    UNREAD_QUERY = 'SELECT ZTITLE, ZURL, ZIDENTIFIER, ZDATE FROM ZPINBOARDPOST WHERE ZDELETING=0 AND ZUNREAD == ?'
    # the public status is 1 or 0
    PUBLIC_QUERY = 'SELECT ZTITLE, ZURL, ZIDENTIFIER, ZDATE FROM ZPINBOARDPOST WHERE ZDELETING=0 AND ZSHARED == ?'

    def _query_specific(self, cursor, query):
        queries = []
        params = []

        def create_and_add_query(sql_query, term):
            # construct a query for each word in the search term
            for word in term.split():
                queries.append(sql_query)
                params.append('%' + word + '%')

        if query.name:
            create_and_add_query(Database.NAME_QUERY, query.name)
        if query.url:
            create_and_add_query(Database.URL_QUERY, query.url)
        if query.desc:
            create_and_add_query(Database.DESC_QUERY, query.desc)

        if query.tags:
            # construct an intersection of queries for each tag
            tag_queries = []
            for tag in query.tags:
                tag_queries.append(Database.TAG_QUERY)
                params.append(tag)
            queries.append(' INTERSECT '.join(tag_queries))

        if query.unread is not None:  # check for None specifically since it can be 0
            queries.append(Database.UNREAD_QUERY)
            params.append(query.unread)

        if query.public is not None:  # check for None specifically since it can be 0
            queries.append(Database.PUBLIC_QUERY)
            params.append(query.public)

        if not queries:
            return None

        sql = ' INTERSECT '.join(queries) + ' ORDER BY ZDATE DESC'
        cursor.execute(sql, params)
        return self._generate_bookmarks(cursor)

    @staticmethod
    def _retrieve_database_path(account_identifier):
        path = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask, True).firstObject()
        path = os.path.join(path,
                            'Group Containers',
                            'Q8B696Y8U4.com.ddeville.spillo',
                            'Library',
                            'Application Support',
                            'Stores',
                            account_identifier,
                            'Core',
                            'Pinboard.sqlite'
                            )
        # attempt to open the file so that we throw if it doesn't exist
        with open(path):
            pass
        return path

    @staticmethod
    def _generate_bookmarks(cursor):
        bookmarks = []
        for row in cursor:
            bookmarks.append(Bookmark(title=row[0], url=row[1], identifier=row[2], date=row[3]))
        return bookmarks


class DatabaseException(Exception):
    pass
