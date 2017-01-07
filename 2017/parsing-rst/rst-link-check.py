# Simple link checker for reStructuredText (.rst) files.
#
# Run with Python 3.4+
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import argparse
import sys
import urllib.error
import urllib.request

import docutils.frontend
import docutils.nodes
import docutils.parsers.rst
import docutils.utils


def check_link(uri):
    """Checks a single URI."""
    print('... Checking {0}...'.format(uri), end='')
    try:
        urllib.request.urlopen(uri, timeout=2.0)
        print('OK')
    except urllib.error.URLError as e:
        print('ERROR: unable to open --', e.reason)
    except Exception as e:
        print('ERROR: exception while opening --', e)


class LinkCheckerVisitor(docutils.nodes.GenericNodeVisitor):
    def visit_reference(self, node):
        # Catch reference nodes for link-checking.
        check_link(node['refuri'])

    def default_visit(self, node):
        # Pass all other nodes through.
        pass


def check_links_in_rst(fileobj):
    # Parse the file into a document with the rst parser.
    default_settings = docutils.frontend.OptionParser(
        components=(docutils.parsers.rst.Parser,)).get_default_values()
    document = docutils.utils.new_document(fileobj.name, default_settings)
    parser = docutils.parsers.rst.Parser()
    parser.parse(fileobj.read(), document)

    # Visit the parsed document with our link-checking visitor.
    visitor = LinkCheckerVisitor(document)
    document.walk(visitor)


def main():
    argparser = argparse.ArgumentParser()
    argparser.add_argument('infile', nargs='?', type=argparse.FileType('r'),
                           default=sys.stdin)
    args = argparser.parse_args()
    print('Reading', args.infile.name)
    check_links_in_rst(args.infile)


if __name__ == '__main__':
    main()
