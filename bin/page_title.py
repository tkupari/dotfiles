#!/usr/bin/env python

import sys
from urllib.request import urlopen
from html.parser import HTMLParser


class TitleParser(HTMLParser):
    def __init__(self):
        HTMLParser.__init__(self)
        self.match = False
        self.title = ''

    def handle_starttag(self, tag, attributes):
        self.match = True if tag == 'title' else False

    def handle_data(self, data):
        if self.match:
            self.title = data
            self.match = False

if len(sys.argv) != 2:
    print("""
Usage:
page_title.py <url>

""")
    sys.exit(1)

url = sys.argv[1]

html_string = str(urlopen(url).read())

parser = TitleParser()
parser.feed(html_string)
print(parser.title)
