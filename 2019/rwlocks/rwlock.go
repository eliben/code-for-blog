// Implementation of several read-write locks in Go.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"context"
	"fmt"
	"sync"
	"sync/atomic"

	"golang.org/x/sync/semaphore"
)

// RWLocker is an interface RW locks are implementing.
type RWLocker interface {
	RLock()
	RUnlock()
	WLock()
	WUnlock()
}

// DummyRWLock is a dummy no-op implementing the RWLocker interface.
// It's not locking anything, so it will demonstrate that tests catch failure.
type DummyRWLock struct {
}

func (*DummyRWLock) RLock()   {}
func (*DummyRWLock) RUnlock() {}
func (*DummyRWLock) WLock()   {}
func (*DummyRWLock) WUnlock() {}

// MutexAsRWLock is an adapter for sync.Mutex to implement the RWLocker
// interface.
// It uses a single mutex for both R and W locking, so it's not efficient, but
// should be correct.
type MutexAsRWLock struct {
	m sync.Mutex
}

func (l *MutexAsRWLock) RLock()   { l.m.Lock() }
func (l *MutexAsRWLock) RUnlock() { l.m.Unlock() }
func (l *MutexAsRWLock) WLock()   { l.m.Lock() }
func (l *MutexAsRWLock) WUnlock() { l.m.Unlock() }

// RWMutexAsRWLock is an adapter for sync.RWMutex to implement the RWLocker
// interface.
type RWMutexAsRWLock struct {
	rwm sync.RWMutex
}

func (l *RWMutexAsRWLock) RLock()   { l.rwm.RLock() }
func (l *RWMutexAsRWLock) RUnlock() { l.rwm.RUnlock() }
func (l *RWMutexAsRWLock) WLock()   { l.rwm.Lock() }
func (l *RWMutexAsRWLock) WUnlock() { l.rwm.Unlock() }

// ReaderCountRWLock uses a single mutex along with a count of readers. Writers
// will spin on the lock until they see there are no readers.
type ReaderCountRWLock struct {
	m           sync.Mutex
	readerCount int
}

func (l *ReaderCountRWLock) RLock() {
	l.m.Lock()
	l.readerCount++
	l.m.Unlock()
}

func (l *ReaderCountRWLock) RUnlock() {
	l.m.Lock()
	l.readerCount--
	if l.readerCount < 0 {
		panic("readerCount negative")
	}
	l.m.Unlock()
}

func (l *ReaderCountRWLock) WLock() {
	for {
		l.m.Lock()
		if l.readerCount > 0 {
			l.m.Unlock()
		} else {
			break
		}
	}
}

func (l *ReaderCountRWLock) WUnlock() {
	l.m.Unlock()
}

// ReaderCountCondRWLock is similar to ReaderCountRWLock, but uses a condition
// variable to wake up waiting writers (instead of spinning).
type ReaderCountCondRWLock struct {
	readerCount int
	c           *sync.Cond
}

// NewReaderCountCondRWLock creates a new ReaderCountRWLock.
func NewReaderCountCondRWLock() *ReaderCountCondRWLock {
	return &ReaderCountCondRWLock{0, sync.NewCond(new(sync.Mutex))}
}

func (l *ReaderCountCondRWLock) RLock() {
	l.c.L.Lock()
	l.readerCount++
	l.c.L.Unlock()
}

func (l *ReaderCountCondRWLock) RUnlock() {
	l.c.L.Lock()
	l.readerCount--
	if l.readerCount < 0 {
		panic("readerCount negative")
	} else if l.readerCount == 0 {
		l.c.Signal()
	}
	l.c.L.Unlock()
}

func (l *ReaderCountCondRWLock) WLock() {
	l.c.L.Lock()
	for l.readerCount > 0 {
		l.c.Wait()
	}
}

func (l *ReaderCountCondRWLock) WUnlock() {
	l.c.Signal()
	l.c.L.Unlock()
}

// SemaRWLock uses a semaphore; each reader acquires one unit and each writer
// acquires the full semaphore.
const maxWeight int64 = 1 << 30

type SemaRWLock struct {
	s *semaphore.Weighted
}

// NewSemaRWLock creates a new SemaRWLock.
func NewSemaRWLock() *SemaRWLock {
	return &SemaRWLock{semaphore.NewWeighted(maxWeight)}
}

func (l *SemaRWLock) RLock() {
	l.s.Acquire(context.Background(), 1)
}

func (l *SemaRWLock) RUnlock() {
	l.s.Release(1)
}

func (l *SemaRWLock) WLock() {
	l.s.Acquire(context.Background(), maxWeight)
}

func (l *SemaRWLock) WUnlock() {
	l.s.Release(maxWeight)
}

// WritePreferRWLock is a writer-preferring RW lock.
// (from https://en.wikipedia.org/wiki/Readers%E2%80%93writer_lock)
type WritePreferRWLock struct {
	readerCount int
	hasWriter   bool
	c           *sync.Cond
}

func NewWritePreferRWLock() *WritePreferRWLock {
	return &WritePreferRWLock{0, false, sync.NewCond(new(sync.Mutex))}
}

func (l *WritePreferRWLock) RLock() {
	l.c.L.Lock()
	// Wait as long as there are writers waiting to take the lock.
	for l.hasWriter {
		l.c.Wait()
	}
	l.readerCount++
	l.c.L.Unlock()
}

func (l *WritePreferRWLock) RUnlock() {
	l.c.L.Lock()
	l.readerCount--
	if l.readerCount == 0 {
		l.c.Signal()
	}
	l.c.L.Unlock()
}

func (l *WritePreferRWLock) WLock() {
	l.c.L.Lock()
	// Wait until there are no other writers contending for the lock, and mark
	// hasWriter = true.
	for l.hasWriter {
		l.c.Wait()
	}
	l.hasWriter = true
	for l.readerCount > 0 {
		l.c.Wait()
	}
	l.c.L.Unlock()
}

func (l *WritePreferRWLock) WUnlock() {
	l.c.L.Lock()
	l.hasWriter = false
	l.c.Broadcast()
	l.c.L.Unlock()
}

// WritePreferFastRWLock is a faster implementation of a RW lock that prefers
// writers. It's similar to the Go standard library implementation of RWMutex,
// but uses channels instead of runtime_Sem* primitives.
type WritePreferFastRWLock struct {
	// w guarantees mutual exclusion between writers.
	w sync.Mutex

	// These channels serve as semaphores for a writer to wait on readers and for
	// readers to wait for a writer, without spinning.
	writerWait chan struct{}
	readerWait chan struct{}

	// numPending marks the number of readers or writers that are still using the
	// lock. Readers increment it by one, but a writer subtracts maxReaders;
	// therefore, a negative value means a writer is currently using the lock.
	numPending int32

	// readersDeparting is used when a writer waits for all existing readers to
	// flush out before doing its thing. When a writer comes in to take a look
	// and guarantees it's the only writer, it posts -maxReaders onto numPending
	// to notify new readers not to enter. It then also posts the number of
	// pending readers onto readersDeparting. Each reader will decrement this
	// field before relinquishing the lock, and the writer waits for all of them
	// before proceeding.
	readersDeparting int32
}

const maxReaders int32 = 1 << 30

func NewWritePreferFastRWLock() *WritePreferFastRWLock {
	var l WritePreferFastRWLock
	l.writerWait = make(chan struct{})
	l.readerWait = make(chan struct{})
	return &l
}

func (l *WritePreferFastRWLock) RLock() {
	if atomic.AddInt32(&l.numPending, 1) < 0 {
		// A writer is pending, wait for it.
		<-l.readerWait
	}
}

func (l *WritePreferFastRWLock) RUnlock() {
	if r := atomic.AddInt32(&l.numPending, -1); r < 0 {
		// If numPending is now negative, it can be either because of an
		// error ...
		if r+1 == 0 || r+1 == -maxReaders {
			panic("RUnlock of unlocked RWLock")
		}
		// ... or because a writer is pending
		if atomic.AddInt32(&l.readersDeparting, -1) == 0 {
			// The last reader unblocks the writer
			l.writerWait <- struct{}{}
		}
	}
}

func (l *WritePreferFastRWLock) WLock() {
	l.w.Lock()
	// Announce to readers there is a pending writer by decrementing maxReaders
	// from numPending. r will hold the number of pending readers.
	r := atomic.AddInt32(&l.numPending, -maxReaders) + maxReaders
	// Wait for active readers
	if r != 0 && atomic.AddInt32(&l.readersDeparting, r) != 0 {
		<-l.writerWait
	}
}

func (l *WritePreferFastRWLock) WUnlock() {
	// Announce to readers there is no longer an active writer.
	r := atomic.AddInt32(&l.numPending, maxReaders)
	if r >= maxReaders {
		panic("WUnlock of unlocked RWLock")
	}
	// Unblock blocked readers, if any.
	for i := 0; i < int(r); i++ {
		l.readerWait <- struct{}{}
	}
	// Allow other writers to proceed.
	l.w.Unlock()
}

func main() {
	fmt.Println("Run tests")
}
