# Using the standard library's xml.etree.ElementTree module.
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
import sys
import xml.etree.ElementTree as ET

count = 0
for event, elem in ET.iterparse(sys.argv[1], events=("end",)):
    if event == "end":
        if elem.tag == 'location' and elem.text and 'Africa' in elem.text:
            count += 1
        elem.clear()

print('count =', count)
