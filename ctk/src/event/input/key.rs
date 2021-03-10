use ncurses::WchResult;
use self::ActionKey::*;
use self::Key::*;
use super::{InputEvent, Modifier::*, Modifiers};

/// An event to be fired when a key is typed. Key events are first
/// sent to the currently focused component. If it doesn't comsume
/// them, its ancestors will recursively receive them. If no
/// components are focused, key events will be discarded.
///
/// Key combinations with modifier keys are poorly detected. No, it's
/// not a GUI toolkit. It's only a terminal. There won't be separate
/// key-up or key-down events, nor distinct events for modifier keys
/// (like when Shift key is pressed).
///
/// Latin alphabets generated with Shift key (i.e. upper-case letters)
/// are reported as if they were from separate keys for such
/// characters, and [InputEvent::is_shift_down()] returns `false` in
/// this case. The same goes for symbols generated with Shift
/// key. That is, when `!` is typed on a US keyboard, it is reported
/// as a key event with character `!` without Shift key being
/// pressed. This is how terminals work and it is impossible to detect
/// it as Shift-1 because users might not be using a US keyboard and
/// terminals tell us nothing about the keyboard layout.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct KeyEvent {
    mods: Modifiers,
    key: Key
}

impl KeyEvent {
    pub fn key_char(&self) -> Option<char> {
        match self.key {
            Char(ch) => Some(ch),
            _        => None
        }
    }
}

impl InputEvent for KeyEvent {
    fn is_shift_down(&self) -> bool {
        self.mods.is_shift_down()
    }

    fn is_control_down(&self) -> bool {
        self.mods.is_control_down()
    }

    fn is_alt_down(&self) -> bool {
        self.mods.is_alt_down()
    }
}

impl From<WchResult> for KeyEvent {
    fn from(wch: WchResult) -> Self {
        /* Decoding key codes is seriously a hard task because there
         * is no standard way to encode key events to escape
         * sequences. There is a pretty good proposal
         * (http://www.leonerd.org.uk/hacks/fixterms/) but of course
         * no one really does this.
         */
        match wch {
            WchResult::KeyCode(c) => {
                decode_keycode(c)
            },
            WchResult::Char(c) => {
                decode_char(c)
            }
        }
    }
}

fn decode_keycode(c: i32) -> KeyEvent {
    /* NCurses recognizes only a small portion of shifted action
     * keys. Detecting key combinations like Ctrl-Right is plain just
     * impossible. Alt *might* be detectable with the help of
     * define_key(3) but that is truly terminal-specific, and
     * ncurses/terminfo doesn't abstract it away. */
    let no_mods = Modifiers::default();
    let shifted = Shift.into();
    match c {
        ncurses::KEY_BREAK     => KeyEvent { mods: no_mods, key: Action(Break     ) },
        ncurses::KEY_DOWN      => KeyEvent { mods: no_mods, key: Action(Down      ) },
        ncurses::KEY_UP        => KeyEvent { mods: no_mods, key: Action(Up        ) },
        ncurses::KEY_LEFT      => KeyEvent { mods: no_mods, key: Action(Left      ) },
        ncurses::KEY_RIGHT     => KeyEvent { mods: no_mods, key: Action(Right     ) },
        ncurses::KEY_HOME      => KeyEvent { mods: no_mods, key: Action(Home      ) },
        ncurses::KEY_BACKSPACE => KeyEvent { mods: no_mods, key: Action(Backspace ) },
        ncurses::KEY_F1        => KeyEvent { mods: no_mods, key: Action(F1        ) },
        ncurses::KEY_F2        => KeyEvent { mods: no_mods, key: Action(F2        ) },
        ncurses::KEY_F3        => KeyEvent { mods: no_mods, key: Action(F3        ) },
        ncurses::KEY_F4        => KeyEvent { mods: no_mods, key: Action(F4        ) },
        ncurses::KEY_F5        => KeyEvent { mods: no_mods, key: Action(F5        ) },
        ncurses::KEY_F6        => KeyEvent { mods: no_mods, key: Action(F6        ) },
        ncurses::KEY_F7        => KeyEvent { mods: no_mods, key: Action(F7        ) },
        ncurses::KEY_F8        => KeyEvent { mods: no_mods, key: Action(F8        ) },
        ncurses::KEY_F9        => KeyEvent { mods: no_mods, key: Action(F9        ) },
        ncurses::KEY_F10       => KeyEvent { mods: no_mods, key: Action(F10       ) },
        ncurses::KEY_F11       => KeyEvent { mods: no_mods, key: Action(F11       ) },
        ncurses::KEY_F12       => KeyEvent { mods: no_mods, key: Action(F12       ) },
        ncurses::KEY_F13       => KeyEvent { mods: no_mods, key: Action(F13       ) },
        ncurses::KEY_F14       => KeyEvent { mods: no_mods, key: Action(F14       ) },
        ncurses::KEY_F15       => KeyEvent { mods: no_mods, key: Action(F15       ) },
        ncurses::KEY_DC        => KeyEvent { mods: no_mods, key: Action(Delete    ) },
        ncurses::KEY_IC        => KeyEvent { mods: no_mods, key: Action(Insert    ) },
        ncurses::KEY_NPAGE     => KeyEvent { mods: no_mods, key: Action(PageDown  ) },
        ncurses::KEY_PPAGE     => KeyEvent { mods: no_mods, key: Action(PageUp    ) },
        ncurses::KEY_BTAB      => KeyEvent { mods: shifted, key: Action(Tab       ) },
        ncurses::KEY_END       => KeyEvent { mods: no_mods, key: Action(End       ) },
        ncurses::KEY_SDC       => KeyEvent { mods: shifted, key: Action(Delete    ) },
        ncurses::KEY_SEND      => KeyEvent { mods: shifted, key: Action(End       ) },
        ncurses::KEY_SHOME     => KeyEvent { mods: shifted, key: Action(Home      ) },
        ncurses::KEY_SIC       => KeyEvent { mods: shifted, key: Action(Insert    ) },
        ncurses::KEY_SLEFT     => KeyEvent { mods: shifted, key: Action(Left      ) },
        ncurses::KEY_SRIGHT    => KeyEvent { mods: shifted, key: Action(Right     ) },
        // Hey... Where is shifted up and down?
        _                      => KeyEvent { mods: no_mods, key: Action(Unknown(c)) }
    }
}

fn decode_char(c: ncurses::winttype) -> KeyEvent {
    let no_mods = Modifiers::default();
    let ctrl    = Control.into();
    match c {
        // Terminals tend to emit characters masked with 0x1F (what??)
        // when typed with Ctrl. Certain combinations like Ctrl-J are
        // indistinguishable from regular keys.
        0x00 => KeyEvent { mods: ctrl   , key: Action(Space ) },
        0x09 => KeyEvent { mods: no_mods, key: Action(Tab   ) }, // No Ctrl-I for you.
        0x0D => KeyEvent { mods: no_mods, key: Action(Return) }, // No Ctrl-M.
        // ESC is the most problematic key in this terrible world. C-[
        // is also eaten by it.
        0x1B => KeyEvent { mods: no_mods, key: Action(Escape) },
        _ if c >= 0x01 && c <= 0x1A => {
            // C-a to C-z (lower-case)
            KeyEvent {
                mods: ctrl,
                key: unsafe { Char(std::mem::transmute(c + 0x60)) }
            }
        },
        _ if c >= 0x1C && c <= 0x1f => {
            // C-\, C-], C-^, and C-_.
            KeyEvent {
                mods: ctrl,
                key: unsafe { Char(std::mem::transmute(c + 0x40)) }
            }
        },
        0x20 => KeyEvent { mods: no_mods, key: Action(Space) },
        // ASCII DEL. Should we count this as Delete? There is a
        // fucking long story behind this fucking key, and there seems
        // to be absolutely no sane way to handle this "correctly."
        0x7F => KeyEvent { mods: no_mods, key: Action(Backspace) },
        // Non-control characters.
        _ =>
            // char::from_u32() is nightly-only. How the fuck are we
            // supposed to do this in stable Rust safely?
            if let Some(ch) = unsafe { Some(std::mem::transmute(c)) } {
                KeyEvent {
                    mods: no_mods,
                    key: Char(ch)
                }
            }
            else {
                KeyEvent {
                    mods: no_mods,
                    key: Action(Unknown(c as i32))
                }
            }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum Key {
    Char(char),
    Action(ActionKey)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum ActionKey {
    /// Break key (unreliable)
    Break,
    /// Arrow Down
    Down,
    /// Arrow Up
    Up,
    /// Arrow Left
    Left,
    /// Arrow Right
    Right,
    Home,
    Backspace,
    /// Function keys, but terminal emulators tend to eat these.
    F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15,

    // NCurses defines a lot of action keys only found on ancient
    // keyboards. Delete-Line, Insert-Line,
    // Exit-Insert-Character-Mode... It makes no sense to support them
    // today as nobody has such keys.

    Delete, // Mapped to KEY_DC (delete-character) on curses
    Insert, // Mapped to KEY_IC (insert-character) on curses
    PageDown,
    PageUp,
    End,

    // NCurses doesn't treat these keys as action keys, but we do it
    // nevertheless.
    Tab,
    Return,
    Escape,
    Space,

    /// Unknown - Ctk doesn't know what key this is. The i32 is a raw
    /// key code value given by ncurses.
    Unknown(i32)
}
