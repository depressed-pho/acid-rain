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
