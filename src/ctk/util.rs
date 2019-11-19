use std::io;

/** Check if a result code from pancurses functions indicates a
 * success or a failure.
 */
pub(crate) fn check(r: i32) -> Result<(), ()> {
    if r == ncurses::ERR {
        Err(())
    }
    else {
        Ok(())
    }
}

pub(crate) fn check_null<T>(p: *mut T) -> Result<*mut T, io::Error> {
    if p.is_null() {
        Err(io::Error::last_os_error())
    }
    else {
        Ok(p)
    }
}
