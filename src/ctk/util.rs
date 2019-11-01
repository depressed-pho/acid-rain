pub(crate) fn check(r: i32) -> Result<(), ()> {
    if r == pancurses::ERR {
        Err(())
    }
    else {
        Ok(())
    }
}
