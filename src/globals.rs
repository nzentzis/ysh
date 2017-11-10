use std::sync::atomic;

static ENABLE_JOB_CONTROL: atomic::AtomicBool = atomic::ATOMIC_BOOL_INIT;

pub fn enable_job_control() {
    ENABLE_JOB_CONTROL.store(true, atomic::Ordering::Relaxed);
}

pub fn job_control() -> bool {
    ENABLE_JOB_CONTROL.load(atomic::Ordering::Relaxed)
}
