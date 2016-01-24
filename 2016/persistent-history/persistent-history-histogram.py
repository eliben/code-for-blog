# Histogram for persistent_history.
#
# This script runs on Python 2 or 3. Run it with --help for directions.
#
# See http://eli.thegreenplace.net/2013/06/11/keeping-persistent-history-in-bash
# for a description of the parsed file.
#
# Eli Bendersky (http://eli.thegreenplace.net)
# This code is in the public domain

from __future__ import print_function
import argparse
from collections import Counter
import os
import sys
import time

DESCRIPTION = '''\
Histogram for .persistent_history commands. All dates are in %Y-%m-%d format.
'''

def parsetime(timestr):
    return time.strptime(timestr, '%Y-%m-%d')

def main():
    argparser = argparse.ArgumentParser(description=DESCRIPTION)
    argparser.add_argument('filename', help='file name to process')
    argparser.add_argument('--start-date',
        help='date to start collecting from; by default, earliest in file')
    argparser.add_argument('--end-date',
        help='date to end collecting from; by default, latest')
    argparser.add_argument('--num-common', type=int, default=10,
        help='Number of most common commands to display')
    args = argparser.parse_args()

    if args.start_date:
        args.start_date = parsetime(args.start_date)

    if args.end_date:
        args.end_date = parsetime(args.end_date)

    cmds = []
    with open(args.filename) as f:
        for line in f:
            fields = line.split()
            if len(fields) < 4 or fields[2] != "|":
                continue
            try:
                date = parsetime(fields[0])
            except ValueError:
                continue
            if args.start_date and date < args.start_date:
                continue
            if args.end_date and date > args.end_date:
                continue
            cmds.append(fields[3].split()[0])

    print('Number of commands processed: ', len(cmds))
    most_common_list = Counter(cmds).most_common(args.num_common)
    maxcmdlen = 1
    for cmd, _ in most_common_list:
        maxcmdlen = max(len(cmd), maxcmdlen)
    for cmd, count in most_common_list:
        print(('%-' + str(maxcmdlen + 1) + 's: %s') % (cmd, count))

if __name__ == '__main__':
    main()
