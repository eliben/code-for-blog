# Test runner for BF executors.
#
# Written for Python 3.5+
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import json
import os
import subprocess
import sys
import time


class Test:
    def __init__(self, name, program_path, params):
        self.name = name
        self.program_path = program_path
        self.params = params


def discover_tests(dir_path):
    """Yields all tests found in the given directory.

    Each yielded object is a Test with its fields initialized.
    """
    for filename in sorted(os.listdir(dir_path)):
        if filename.endswith('.bf'):
            testname = os.path.splitext(filename)[0]
            program_path = os.path.join(dir_path, filename)

            params_path = os.path.join(dir_path, testname + '.test')

            try:
              params = json.load(open(params_path))
            except:
              print('ERROR while parsing json from {0}'.format(params_path))
              raise

            yield Test(testname, program_path, params)


def run_all_tests(executor_path, flags, tests_dir_path):
    """Runs all tests found in tests_dir_path with the given executor.

    Flags is a list of command-line flags to pass to the executor.

    The executor is a program that can be invoked given a test file name as
    a single parameter. It reads from stdin and writes to stdout.
    """
    print('\nTesting {0} {1}'.format(executor_path, ' '.join(flags)))
    starttime = time.time()
    errorcount = 0

    for i, test in enumerate(discover_tests(dir_path=tests_dir_path), start=1):
        print('Running test #{:0>3} {:.<30}'.format(i, '[' + test.name + ']'),
              end='')

        # Figure out what to feed into stdin and what output to expect; encode
        # them as bytes.
        stdin_feed = test.params.get('feed-in', '').encode('utf-8')
        expected_out = test.params.get('expect-out', '').encode('utf-8')

        try:
            subproc = subprocess.run(
                [executor_path] + flags + [test.program_path],
                timeout=1,
                stdout=subprocess.PIPE,
                input=stdin_feed)

            if subproc.returncode != 0:
              errorcount += 1
              print('ERROR')
              print('---- Executor returned: {0}'.format(subproc.returncode))
              print('---- Output: {0}'.format(subproc.stdout))
            elif subproc.stdout != expected_out:
              errorcount += 1
              print('ERROR')
              print('---- Expected output: {0}'.format(expected_out))
              print('---- Output: {0}'.format(subproc.stdout))
            else:
              print('OK')
        except subprocess.TimeoutExpired as e:
            errorcount += 1
            print('ERROR -- timeout')

    if errorcount == 0:
        print('---- All tests ran OK ----')
    else:
        print('---- Tests had %s errors ----' % errorcount)

    print('Elapsed: %.4s sec' % (time.time() - starttime,))


if __name__ == '__main__':
    testcases_path = 'tests/testcases'
    if not os.path.exists(testcases_path):
        print('ERROR: cannot find "{}"'.format(testcases_path))
        print('Run me from the main directory!')
        sys.exit(1)
    for executor in (
            './simpleinterp',
            './optinterp',
            './optinterp2',
            './optinterp3'):
        run_all_tests(executor, [], testcases_path)
