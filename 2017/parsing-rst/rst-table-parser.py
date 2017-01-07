# Parsing of grid tables using docutils's GridTableParser.
#
# Run with Python 3.4+
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import docutils.statemachine
import docutils.parsers.rst.tableparser


def parse_grid_table(text):
    # Clean up the input: get rid of empty lines and strip all leading and
    # trailing whitespace.
    lines = filter(bool, (line.strip() for line in text.splitlines()))
    parser = docutils.parsers.rst.tableparser.GridTableParser()
    return parser.parse(docutils.statemachine.StringList(list(lines)))


if __name__ == '__main__':
    tablestr = '''
        +------------------------+------------+----------+----------+
        | Header row, column 1   | Header 2   | Header 3 | Header 4 |
        +========================+============+==========+==========+
        | body row 1, column 1   | column 2   | column 3 | column 4 |
        +------------------------+------------+----------+----------+
        | body row 2             | Cells may span columns.          |
        +------------------------+------------+---------------------+
        | body row 3             | Cells may  | - Table cells       |
        +------------------------+ span rows. | - contain           |
        | body row 4             |            | - body elements.    |
        +------------------------+------------+---------------------+
    '''

    parsed_table = parse_grid_table(tablestr)

    import pprint
    pprint.pprint(parsed_table)
