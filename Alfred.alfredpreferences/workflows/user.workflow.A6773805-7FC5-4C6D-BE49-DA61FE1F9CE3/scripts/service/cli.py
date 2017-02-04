from spillo.emitter import Emitter


class CLIEmitter(Emitter):
    def generate_output(self, bookmarks):
        if not bookmarks:
            return ""
        else:
            output = ""
            for bookmark in bookmarks:
                output += bookmark.title
                output += ' - '
                output += bookmark.url
                output += '\n'
            return output
