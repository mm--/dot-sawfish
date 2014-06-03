#!/usr/bin/env python
# Take in a URL and print out the main text
import sys

from goose import Goose

url = str(sys.argv[1])
g = Goose()
article = g.extract(url=url)
print(article.title.encode('utf-8'))
print(article.cleaned_text.encode('utf-8'))

