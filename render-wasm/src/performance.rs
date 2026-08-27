use std::sync::atomic::{AtomicBool, AtomicI32, AtomicUsize, Ordering};

#[cfg(target_arch = "wasm32")]
pub fn get_time() -> i32 {
    crate::get_now!() as i32
}

#[cfg(not(target_arch = "wasm32"))]
pub fn get_time() -> i32 {
    let now = std::time::Instant::now();
    now.elapsed().as_millis() as i32
}

/// Always-on page-load / page-switch tracing (temporary profiling).
/// Prints to stdout (browser console under Emscripten). No feature gates.
/// Active only between `page_trace_reset` and first tiles-complete to avoid
/// flooding the console on pan/zoom.
static PAGE_TRACE_ACTIVE: AtomicBool = AtomicBool::new(false);
static PAGE_TRACE_EPOCH: AtomicI32 = AtomicI32::new(0);
static PAGE_TRACE_LAST: AtomicI32 = AtomicI32::new(0);
static PAGE_TRACE_USE_SHAPE: AtomicUsize = AtomicUsize::new(0);

pub fn page_trace_active() -> bool {
    PAGE_TRACE_ACTIVE.load(Ordering::Relaxed)
}

/// Start a new page-load/switch session. Resets epoch and counters.
pub fn page_trace_reset(label: &str) {
    let now = get_time();
    PAGE_TRACE_ACTIVE.store(true, Ordering::Relaxed);
    PAGE_TRACE_EPOCH.store(now, Ordering::Relaxed);
    PAGE_TRACE_LAST.store(now, Ordering::Relaxed);
    PAGE_TRACE_USE_SHAPE.store(0, Ordering::Relaxed);
    println!("[wasm-page] === {label} === t={now}ms");
}

/// Log a milestone with delta since previous mark and since session start.
pub fn page_trace(label: &str) {
    if !PAGE_TRACE_ACTIVE.load(Ordering::Relaxed) {
        return;
    }
    let now = get_time();
    let epoch = PAGE_TRACE_EPOCH.load(Ordering::Relaxed);
    let last = PAGE_TRACE_LAST.load(Ordering::Relaxed);
    let since_epoch = if epoch == 0 { 0 } else { now - epoch };
    let since_last = if last == 0 { 0 } else { now - last };
    PAGE_TRACE_LAST.store(now, Ordering::Relaxed);
    println!("[wasm-page] {label} +{since_last}ms (session +{since_epoch}ms) t={now}ms");
}

/// Count a `use_shape` during the active page-load session.
pub fn page_trace_use_shape() {
    if PAGE_TRACE_ACTIVE.load(Ordering::Relaxed) {
        PAGE_TRACE_USE_SHAPE.fetch_add(1, Ordering::Relaxed);
    }
}

/// Log end of bulk loading with shape count.
pub fn page_trace_end_loading() {
    let n = PAGE_TRACE_USE_SHAPE.load(Ordering::Relaxed);
    page_trace(&format!("end_loading shapes={n}"));
}

/// Final milestone of the page session (first tiles-complete), then silence.
pub fn page_trace_done(label: &str) {
    page_trace(label);
    PAGE_TRACE_ACTIVE.store(false, Ordering::Relaxed);
}

/// Log a message to the browser console (only when profile-macros feature is enabled)
#[macro_export]
macro_rules! console_log {
    ($($arg:tt)*) => {
        #[cfg(all(feature = "profile-macros", target_arch = "wasm32"))]
        {
            use $crate::run_script;
            run_script!(format!("console.log('{}')", format!($($arg)*)));
        }
        #[cfg(all(feature = "profile-macros", not(target_arch = "wasm32")))]
        {
            println!($($arg)*);
        }
    };
}

/// Begin a timed section with logging (only when profile-macros feature is enabled)
/// Returns the start time - store it and pass to end_timed_log!
#[macro_export]
macro_rules! begin_timed_log {
    ($name:expr) => {{
        #[cfg(feature = "profile-macros")]
        {
            $crate::performance::get_time()
        }
        #[cfg(not(feature = "profile-macros"))]
        {
            0.0
        }
    }};
}

/// End a timed section and log the duration (only when profile-macros feature is enabled)
#[macro_export]
macro_rules! end_timed_log {
    ($name:expr, $start:expr) => {{
        #[cfg(all(feature = "profile-macros", target_arch = "wasm32"))]
        {
            let duration = $crate::performance::get_time() - $start;
            use $crate::run_script;
            run_script!(format!(
                "console.log('[PERF] {}: {:.2}ms')",
                $name, duration
            ));
        }
        #[cfg(all(feature = "profile-macros", not(target_arch = "wasm32")))]
        {
            let duration = $crate::performance::get_time() - $start;
            println!("[PERF] {}: {:.2}ms", $name, duration);
        }
    }};
}

#[allow(unused_imports)]
pub use console_log;

#[allow(unused_imports)]
pub use begin_timed_log;

#[allow(unused_imports)]
pub use end_timed_log;

#[macro_export]
macro_rules! mark {
    ($name:expr) => {
        #[cfg(all(feature = "profile-macros", target_arch = "wasm32"))]
        {
            use $crate::run_script;
            run_script!(format!("performance.mark('{}')", $name));
        }
    };
}

#[macro_export]
macro_rules! measure {
    ($name:expr) => {
        #[cfg(all(feature = "profile-macros", target_arch = "wasm32"))]
        {
            use $crate::run_script;
            run_script!(format!("performance.measure('{}')", $name));
        }
    };
    ($name:expr, $mark_begin:expr) => {
        #[cfg(all(feature = "profile-macros", target_arch = "wasm32"))]
        {
            use $crate::run_script;
            run_script!(format!(
                "performance.measure('{}','{}')",
                $name, $mark_begin
            ));
        }
    };
    ($name:expr, $mark_begin:expr, $mark_end:expr) => {
        #[cfg(all(feature = "profile-macros", target_arch = "wasm32"))]
        {
            use $crate::run_script;
            run_script!(format!(
                "performance.measure('{}','{}','{}')",
                $name, $mark_begin, $mark_end
            ));
        }
    };
}

#[macro_export]
macro_rules! begin_mark_name {
    ($name:expr) => {
        format!("{}::begin", $name)
    };
}

#[macro_export]
macro_rules! end_mark_name {
    ($name:expr) => {
        format!("{}::end", $name)
    };
}

#[macro_export]
macro_rules! measure_marks {
    ($name:expr) => {
        #[cfg(all(feature = "profile-macros", target_arch = "wasm32"))]
        {
            use $crate::{begin_mark_name, end_mark_name, measure};
            measure!($name, begin_mark_name!($name), end_mark_name!($name));
        }
    };
}

#[macro_export]
macro_rules! clear_marks {
    () => {
        use $crate::run_script;
        run_script!("performance.clearMarks()");
    };
    ($($name:expr),*) => {
        format!("{}", [$(format!("performance.clearMarks('{}')", $name)),*].join("; "))
    };
}

#[macro_export]
macro_rules! clear_measures {
    () => {
        use $crate::run_script;
        run_script!("performance.clearMeasures()");
    };
    ($($name:expr),*) => {
        format!("{}", [$(format!("performance.clearMeasures('{}')", $name)),*].join("; "))
    };
}

#[macro_export]
macro_rules! begin_measure {
    ($name:expr) => {
        #[cfg(all(feature = "profile-macros", target_arch = "wasm32"))]
        {
            use $crate::{begin_mark_name, mark};
            mark!(begin_mark_name!($name));
        }
    };
    ($name:expr, $clear_marks:expr) => {
        #[cfg(all(feature = "profile-macros", target_arch = "wasm32"))]
        {
            use $crate::{begin_mark_name, clear_marks, end_mark_name, mark};
            if $clear_marks {
                clear_marks!(begin_mark_name!($name), end_mark_name($name));
            }
            mark!(begin_mark_name!($name));
        }
    };
}

#[macro_export]
macro_rules! end_measure {
    ($name:expr) => {
        #[cfg(all(feature = "profile-macros", target_arch = "wasm32"))]
        {
            use $crate::{end_mark_name, mark, measure_marks};
            mark!(end_mark_name!($name));
            measure_marks!($name);
        }
    };
    ($name:expr, $clear_marks:expr) => {
        #[cfg(all(feature = "profile-macros", target_arch = "wasm32"))]
        {
            use $crate::{begin_mark_name, clear_marks, end_mark_name, mark, measure_marks};
            mark!(end_mark_name!($name));
            measure_marks!($name);
            if $clear_marks {
                clear_marks!(begin_mark_name!($name), end_mark_name($name));
            }
        }
    };
}

// We need to reexport the macro to make it public.
#[allow(unused_imports)]
pub use clear_marks;

#[allow(unused_imports)]
pub use clear_measures;

#[allow(unused_imports)]
pub use mark;

#[allow(unused_imports)]
pub use measure;

#[allow(unused_imports)]
pub use begin_measure;

#[allow(unused_imports)]
pub use end_measure;
