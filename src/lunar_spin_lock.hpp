#ifndef LUNAR_SPIN_LOCK_HPP
#define LUNAR_SPIN_LOCK_HPP

#include "lunar_rtm_lock.hpp"

namespace lunar {

class spin_lock_ac;

class spin_lock {
public:
    spin_lock() : m_lock(0) { }
    ~spin_lock() { }

private:
    volatile int m_lock;

    friend class spin_lock_acquire;
    friend class spin_lock_acquire_unsafe;
};

class spin_lock_acquire {
public:
    spin_lock_acquire(spin_lock &lock) : m_spin_lock(lock)
    {
        while (__sync_lock_test_and_set(&lock.m_lock, 1)) {
            while (lock.m_lock)
                _MM_PAUSE(); // busy-wait
        }
    }

    ~spin_lock_acquire()
    {
        __sync_lock_release(&m_spin_lock.m_lock);
    }

private:
    spin_lock &m_spin_lock;
};

class spin_lock_acquire_unsafe {
public:
    spin_lock_acquire_unsafe(spin_lock &lock) : m_spin_lock(lock)
    {
        while (__sync_lock_test_and_set(&lock.m_lock, 1)) {
            while (lock.m_lock)
                _MM_PAUSE(); // busy-wait
        }
    }

    void unlock()
    {
        __sync_lock_release(&m_spin_lock.m_lock);
    }

private:
    spin_lock &m_spin_lock;
};

}

#endif // LUNAR_SPIN_LOCK_HPP
