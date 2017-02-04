#!/usr/bin/env python

import getopt
import sys

from service.alfred import AlfredEmitter
from service.cli import CLIEmitter

from spillo.account import retrieve_account_identifiers
from spillo.database import Database, DatabaseException
from spillo.query import Query, QueryException


def main(argv):
    def _emit(output_str):
        sys.stdout.write(output_str + '\n')

    def _emit_message_and_exit(message, exit_code=1):
        _emit(message)
        sys.exit(exit_code)

    # attempt to parse the command line arguments
    try:
        s, q = _parse_arguments(argv)
    except RuntimeError as e:
        _emit_message_and_exit(str(e))
        return

    # get the right emitter based on the service
    if s == 'cli':
        emitter = CLIEmitter()
    elif s == 'alfred':
        emitter = AlfredEmitter()
    else:
        _emit_message_and_exit('unknown service ' + s)
        return

    # parse the query and emit an empty response if there is none
    try:
        query = Query.parse_query(q)
    except QueryException:
        _emit_message_and_exit(emitter.generate_empty(), 0)
        return

    # retrieve the first account identifier
    account_identifiers = retrieve_account_identifiers()
    if account_identifiers is None or len(account_identifiers) == 0:
        _emit_message_and_exit('No account set up in Spillo')

    # create a database, query it and generate some output via the emitter
    try:
        database = Database(account_identifiers[0])
        output = emitter.generate_output(database.query(query))
    except IOError:
        output = emitter.generate_error('Cannot find Spillo database, make sure that Spillo is installed')
    except DatabaseException:
        output = emitter.generate_error('There was an unknown error while querying the database')

    _emit(output)


def _parse_arguments(argv):
    """Parse the command line arguments and returns the query"""
    usage = 'spillo.py -s <service> -q <query>'

    try:
        opts, args = getopt.getopt(argv, 's:q:', ['service=', 'query='])
    except getopt.GetoptError:
        raise RuntimeError(usage)

    query = None
    service = None

    for opt, arg in opts:
        if opt in ('-s', '--service'):
            service = arg
        elif opt in ('-q', '--query'):
            query = arg

    if service is None or query is None:
        raise RuntimeError(usage)

    return service, query

if __name__ == "__main__":
    main(sys.argv[1:])
