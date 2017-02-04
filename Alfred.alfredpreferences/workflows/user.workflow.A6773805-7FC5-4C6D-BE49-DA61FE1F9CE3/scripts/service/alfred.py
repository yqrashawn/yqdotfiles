from spillo.emitter import Emitter

from xml.etree.ElementTree import (
    Element,
    SubElement,
    tostring,
)


class AlfredEmitter(Emitter):
    def generate_empty(self):
        return self.generate_output([])

    def generate_output(self, bookmarks):
        items_element = Element('items')

        if not bookmarks:
            item_element = SubElement(items_element, 'item', {'valid': 'NO'})

            title_element = SubElement(item_element, 'title')
            title_element.text = 'No Results'

            subtitle_element = SubElement(item_element, 'subtitle')
            subtitle_element.text = 'Could not find any bookmark matching your search query'
        else:
            for bookmark in bookmarks:
                item_element = SubElement(items_element, 'item', {'arg': bookmark.url, 'uid': bookmark.identifier})

                title_element = SubElement(item_element, 'title')
                title_element.text = bookmark.title

                subtitle_element = SubElement(item_element, 'subtitle')
                subtitle_element.text = bookmark.url

                subtitle_alt_element = SubElement(item_element, 'subtitle', {'mod': 'cmd'})
                subtitle_alt_element.text = 'Open bookmark in Spillo'

                subtitle_alt_element = SubElement(item_element, 'subtitle', {'mod': 'alt'})
                subtitle_alt_element.text = 'Open URL in the background'

                icon_element = SubElement(item_element, 'icon')
                icon_element.text = 'document.png'

                copy_element = SubElement(item_element, 'text', {'type': 'copy'})
                copy_element.text = bookmark.url

                largetype_element = SubElement(item_element, 'text', {'type': 'largetype'})
                largetype_element.text = bookmark.title

        return tostring(items_element)

    def generate_error(self, error):
        items_element = Element('items')

        item_element = SubElement(items_element, 'item', {'valid': 'NO'})

        title_element = SubElement(item_element, 'title')
        title_element.text = 'There was an error while querying Spillo'

        subtitle_element = SubElement(item_element, 'subtitle')
        subtitle_element.text = error

        return tostring(items_element)
