// Tests for RW locks.
//
// TestDummy fails, so run specific tests with -run
//
// To run all tests except Dummy:
//   $ SKIPDUMMY=1 go test -v
//
// To print timing, run with TIMING=1
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"math/rand"
	"os"
	"sync"
	"testing"
	"time"
)

// Stats collects statistics for timing.
type Stats struct {
	m     sync.Mutex
	count int64
	total int64
}

func (s Stats) String() string {
	return fmt.Sprintf("count = %d, total = %d, avg = %.2f",
		s.count, s.total, float64(s.total)/float64(s.count))
}

// writer increments every element of data by inc.
// iters specifies how many iterations to run
func writer(iters int, rwl RWLocker, data []int, inc int, wg *sync.WaitGroup, stats *Stats) {
	for i := 0; i < iters; i++ {
		time.Sleep(time.Duration(rand.Intn(8)+1) * 10 * time.Microsecond)
		t1 := time.Now()
		rwl.WLock()

		elapsed := time.Since(t1)
		stats.m.Lock()
		stats.count++
		stats.total += int64(elapsed)
		stats.m.Unlock()

		for i, v := range data {
			data[i] = v + inc
		}
		rwl.WUnlock()
	}
	wg.Done()
}

// reader checks invariant on data: values in ascending order by 1
func reader(iters int, rwl RWLocker, data []int, wg *sync.WaitGroup, stats *Stats) {
	for i := 0; i < iters; i++ {
		time.Sleep(time.Duration(rand.Intn(8)+1) * 10 * time.Microsecond)
		t1 := time.Now()
		rwl.RLock()

		elapsed := time.Since(t1)
		stats.m.Lock()
		stats.count++
		stats.total += int64(elapsed)
		stats.m.Unlock()

		for i := 1; i < len(data); i++ {
			if data[i] != data[i-1]+1 {
				panic("Mismatch")
			}
		}
		rwl.RUnlock()
	}
	wg.Done()
}

// runRWLockTests runs the stress test using rwl as the RW lock.
func runRWLockTests(t *testing.T, rwl RWLocker) {
	rand.Seed(time.Now().UnixNano())
	// Populate initial data with a valid increasing sequence.
	data := make([]int, 1000)
	for i := 0; i < len(data); i++ {
		data[i] = i
	}

	var rStats Stats
	var wStats Stats
	var wg sync.WaitGroup

	// Launch many concurrent readers and several concurrent writers.
	for i := 0; i < 1000; i++ {
		wg.Add(1)
		go reader(100, rwl, data, &wg, &rStats)
	}

	for i := 0; i < 10; i++ {
		wg.Add(1)
		go writer(100, rwl, data, i+1, &wg, &wStats)
	}

	wg.Wait()
	if len(os.Getenv("TIMING")) > 0 {
		fmt.Printf("Reader stats: %s\n", rStats)
		fmt.Printf("Writer stats: %s\n", wStats)
	}
}

// This test fails
func TestDummy(t *testing.T) {
	if len(os.Getenv("SKIPDUMMY")) > 0 {
		t.Skip("Skipping Dummy")
	}
	runRWLockTests(t, &DummyRWLock{})
}

func TestSimpleMutex(t *testing.T) {
	runRWLockTests(t, &MutexAsRWLock{})
}

func TestRWMutex(t *testing.T) {
	runRWLockTests(t, &RWMutexAsRWLock{})
}

func TestReaderCountRWLock(t *testing.T) {
	runRWLockTests(t, &ReaderCountRWLock{})
}

func TestReaderCountCondRWLock(t *testing.T) {
	runRWLockTests(t, NewReaderCountCondRWLock())
}

func TestSema(t *testing.T) {
	runRWLockTests(t, NewSemaRWLock())
}

func TestWritePreferRWLock(t *testing.T) {
	runRWLockTests(t, NewWritePreferRWLock())
}

func TestWritePreferFastRWLock(t *testing.T) {
	runRWLockTests(t, NewWritePreferFastRWLock())
}
