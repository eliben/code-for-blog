# Distributed multiprocessing with Python.
#
# In one terminal run this script without arguments - server.
# In another terminal, run it in client mode as follows:
#
#   $ python <scriptname> client
#
# The client will find the server and get tasks from it; the server will exit
# when all tasks are finished and verified.
#
# Tested with Python 2.7 and 3.6
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
from __future__ import print_function
import sys
PY3 = sys.version_info[0] == 3

import multiprocessing
from multiprocessing.managers import SyncManager
import time

# Python 2 vs. 3 compatibility
if PY3:
    from functools import reduce
    import queue
else:
    import Queue as queue

def iteritems(d):
    """Return an iterator over the items of a dictionary."""
    return getattr(d, 'items' if PY3 else 'iteritems')()


IP = 'localhost'
PORTNUM = 55444
AUTHKEY = b'shufflin'


def factorize_naive(n):
    """ A naive factorization method. Take integer 'n', return list of
        factors.
    """
    if n < 2:
        return []
    factors = []
    p = 2

    while True:
        if n == 1:
            return factors

        r = n % p
        if r == 0:
            factors.append(p)
            n = n // p
        elif p * p >= n:
            factors.append(n)
            return factors
        elif p > 2:
            # Advance in steps of 2 over odd numbers
            p += 2
        else:
            # If p == 2, get to 3
            p += 1
    assert False, "unreachable"


def make_server_manager(port, authkey):
    job_q = queue.Queue()
    result_q = queue.Queue()

    class JobQueueManager(SyncManager):
        pass

    JobQueueManager.register('get_job_q', callable=lambda: job_q)
    JobQueueManager.register('get_result_q', callable=lambda: result_q)

    manager = JobQueueManager(address=('', port), authkey=authkey)
    manager.start()
    print('Server started at port %s' % port)
    return manager


def make_client_manager(ip, port, authkey):
    class ServerQueueManager(SyncManager):
        pass

    ServerQueueManager.register('get_job_q')
    ServerQueueManager.register('get_result_q')

    manager = ServerQueueManager(address=(ip, port), authkey=authkey)
    manager.connect()

    print('Client connected to %s:%s' % (ip, port))
    return manager


def factorizer_worker(job_q, result_q):
    myname = multiprocessing.current_process().name
    while True:
        try:
            job = job_q.get_nowait()
            print('%s got %s nums...' % (myname, len(job)))
            outdict = {n: factorize_naive(n) for n in job}
            result_q.put(outdict)
            print('  %s done' % myname)
        except queue.Empty:
            return


def mp_factorizer(shared_job_q, shared_result_q, nprocs):
    procs = []
    for i in range(nprocs):
        p = multiprocessing.Process(
                target=factorizer_worker,
                args=(shared_job_q, shared_result_q))
        procs.append(p)
        p.start()

    for p in procs:
        p.join()


def make_nums(N):
    nums = [999999999999]
    for i in range(N):
        nums.append(nums[-1] + 2)
    return nums


def runserver():
    manager = make_server_manager(PORTNUM, AUTHKEY)
    shared_job_q = manager.get_job_q()
    shared_result_q = manager.get_result_q()

    N = 999
    nums = make_nums(N)

    chunksize = 43
    for i in range(0, len(nums), chunksize):
        shared_job_q.put(nums[i:i + chunksize])

    numresults = 0
    resultdict = {}
    while numresults < N:
        outdict = shared_result_q.get()
        resultdict.update(outdict)
        numresults += len(outdict)

    for num, factors in iteritems(resultdict):
        product = reduce(lambda a, b: a * b, factors, 1)
        if num != product:
            assert False, "Verification failed for number %s" % num

    print('--- DONE ---')
    time.sleep(2)
    manager.shutdown()


def runclient():
    manager = make_client_manager(IP, PORTNUM, AUTHKEY)
    job_q = manager.get_job_q()
    result_q = manager.get_result_q()

    mp_factorizer(job_q, result_q, 4)


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == 'client':
        runclient()
    else:
        runserver()
