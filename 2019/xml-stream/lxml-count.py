# Using lxml.
# Make sure to install lxml first. I run this from a virtualenv where lxml is
# installed with pip.
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
import sys
from lxml import etree

count = 0
for event, elem in etree.iterparse(sys.argv[1], events=("end",)):
    if event == "end":
        if elem.tag == 'location' and elem.text and 'Africa' in elem.text:
            count += 1
        elem.clear()

print('count =', count)
