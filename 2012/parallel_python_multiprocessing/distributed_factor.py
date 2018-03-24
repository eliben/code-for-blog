# Python 2
import multiprocessing
from multiprocessing.managers import SyncManager
import Queue
import time
from factorize import factorize_naive

IP = 'localhost'
PORTNUM = 55444
AUTHKEY = 'shufflin'


def make_server_manager(port, authkey):
    job_q = Queue.Queue()
    result_q = Queue.Queue()

    class JobQueueManager(SyncManager):
        pass

    JobQueueManager.register('get_job_q', callable=lambda: job_q)
    JobQueueManager.register('get_result_q', callable=lambda: result_q)

    manager = JobQueueManager(address=('', port), authkey=authkey)
    manager.start()
    print 'Server started at port %s' % port
    return manager


def make_client_manager(ip, port, authkey):
    class ServerQueueManager(SyncManager):
        pass

    ServerQueueManager.register('get_job_q')
    ServerQueueManager.register('get_result_q')

    manager = ServerQueueManager(address=(ip, port), authkey=authkey)
    manager.connect()

    print 'Client connected to %s:%s' % (ip, port)
    return manager


def factorizer_worker(job_q, result_q):
    myname = multiprocessing.current_process().name
    while True:
        try:
            job = job_q.get_nowait()
            print '%s got %s nums...' % (myname, len(job))
            outdict = {n: factorize_naive(n) for n in job}
            result_q.put(outdict)
            print '  %s done' % myname
        except Queue.Empty:
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
    for i in xrange(N):
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

    for num, factors in resultdict.iteritems():
        product = reduce(lambda a, b: a * b, factors, 1)
        if num != product:
            assert False, "Verification failed for number %s" % num

    print '--- DONE ---'
    time.sleep(2)
    manager.shutdown()


def runclient():
    manager = make_client_manager(IP, PORTNUM, AUTHKEY)
    job_q = manager.get_job_q()
    result_q = manager.get_result_q()

    mp_factorizer(job_q, result_q, 4)


if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1 and sys.argv[1] == 'client':
        runclient()
    else:
        runserver()
