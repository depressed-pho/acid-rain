use crate::color::RGBColor;
use crate::util::check;
use lru::LruCache;
use scan_fmt::scan_fmt;
use std::convert::TryInto;

#[cfg(feature = "extended-colors")]
type PairIndex = i32;

#[cfg(not(feature = "extended-colors"))]
type PairIndex = i16;

#[cfg(feature = "extended-colors")]
type PaletteIndex = i32;

#[cfg(not(feature = "extended-colors"))]
type PaletteIndex = i16;

/// In order to efficiently use color palettes, we search for a
/// sufficently similar color in a palette before defining a new
/// color. This constant is a threshold of the euclidean distance
/// between two RGB colors.
const SIMILARITY_THRESHOLD: f64 = 0.2;

#[derive(Debug)]
enum TermType {
    NoColors,
    PalettedColor {
        pairs: LruCache<PairIndex, PaletteIndex>,
        palette: LruCache<PaletteIndex, RGBColor>,
        is_mutable: bool
    } ,
    DirectColor {
        /// 'None' represents the default color.
        pairs: LruCache<PairIndex, Option<RGBColor>>,
        r_bits: u8,
        g_bits: u8,
        b_bits: u8
    }
}

/// Color support on terminal is a can of worms. Literally every
/// terminal implements colors in a different way.
///
/// * Not all terminals support colors.
///
/// * Your terminal may only support predefined 16 colors (ANSI),
///   paletted 256 colors, or 24-bits RGB.
///
/// * NCurses does not really support the 16 colors mode. Its built-in
///   palette only has 8 ANSI colors.
///
/// * Some terminals allow applications to directly specify an RGB
///   color as a per-character attribute, while many do not. NCurses
///   also doesn't support this at all. It requires that we define a
///   pair of a background color and a foreground color before using
///   any colors.
///
/// * Some terminals *require* that we define a color palette before
///   defining pairs, while others *require* that we instead define
///   pairs with a direct RGB color. NCurses supports both, but it
///   provides no API to determine which mode the terminal supports
///   (wow). We must query the terminfo capability "RGB" to do it.
///
/// * Even if your terminal supports direct RGB colors, NCurses
///   doesn't tell us how to pack an RGB color into a single int. We
///   must examine that ourselves by again querying "RGB".
///
/// * Terminals that support paletted 256 colors may lack the support
///   for modifying the palette, but there are no portable way to
///   acquire the very palette. In which case the only sane way to go
///   is to give up the 256 colors capability and fall back to ANSI 8
///   colors mode.
///
/// What we want to do is to expose only 24-bits RGB as the API, and
/// translate such colors for your terminal in whatever way it
/// likes. This means we have to deal with color pair indices and also
/// have to approximate RGB colors to a possibly limited set of
/// available colors.
///
/// See also:
///
/// * [How should applications use direct-color
///   mode?](https://lists.gnu.org/archive/html/bug-ncurses/2018-04/msg00011.html)
///
/// * [Support truecolour output with
///   ncurses](https://github.com/mawww/kakoune/issues/1807)
///
/// * [24-bit true color support](https://github.com/weechat/weechat/issues/1364)
///
/// * [termstandard/colors: Color standards for terminal
///   emulators](https://github.com/termstandard/colors/)
///
#[derive(Debug)]
pub(crate) struct ColorManager {
    ttype: TermType,
    has_default_colors: bool
}

impl ColorManager {
    /// Construct a color manager. This has to be done in the same
    /// thread which have instantiated Ctk.
    pub fn new() -> Result<Self, ()> {
        let has_colors = ncurses::has_colors();
        if has_colors {
            /* We are going to use colors. At first we define no color
             * pairs, and progressively define them when needed. */
            check(ncurses::start_color())?;
        }

        let ttype = {
            if has_colors {
                if let Some((r_bits, g_bits, b_bits)) = detect_direct_color() {
                    TermType::DirectColor {
                        pairs: LruCache::new(ncurses::COLOR_PAIRS().try_into().unwrap()),
                        r_bits,
                        g_bits,
                        b_bits
                    }
                }
                else {
                    TermType::PalettedColor {
                        pairs: LruCache::new(ncurses::COLOR_PAIRS().try_into().unwrap()),
                        palette: LruCache::new(ncurses::COLORS().try_into().unwrap()),
                        is_mutable: ncurses::can_change_color()
                    }
                }
            }
            else {
                TermType::NoColors
            }
        };

        let has_default_colors = {
            if has_colors {
                /* Applications may want to use default_colors(3). See
                 * if it's supported by the terminal. */
                ncurses::use_default_colors() == ncurses::OK
            }
            else {
                false
            }
        };

        Ok(Self {
            ttype,
            has_default_colors
        })
    }
}

// This is basically what ncurses/lib_color.c (init_direct_colors)
// does.
fn detect_direct_color() -> Option<(u8, u8, u8)> {
    if ncurses::COLORS() >= 8 {
        // Find the number of bits needed for the maximum color value.
        let mut width = 0;
        while 1 << width < ncurses::COLORS() {
            width += 1;
        }

        match ncurses::tigetflag("RGB") {
            n if n > 0 => {
                let bits   = (width + 2) / 3;
                let r_bits = bits;
                let g_bits = bits;
                let b_bits = width - 2 * bits;
                Some((r_bits, g_bits, b_bits))
            },
            _ => {
                match ncurses::tigetnum("RGB") {
                    n if n > 0 => {
                        let bits = n.try_into().unwrap();
                        Some((bits, bits, bits))
                    },
                    _ => {
                        if let Some(s) = safe_tigetstr("RGB") {
                            Some(
                                scan_fmt!(
                                    &s,
                                    "{d}/{d}/{d}",
                                    u8, u8, u8).expect("Cannot parse the RGB capability"))
                        }
                        else {
                            None
                        }
                    }
                }
            }
        }
    }
    else {
        None
    }
}

// ncurses::tigetstr() assumes that tigetstr() always returns a valid
// string. It's not the case.
fn safe_tigetstr(capname: &str) -> Option<String> {
    use libc::c_char;
    use std::ffi::{CString, CStr};

    unsafe {
        let cap_c = CString::new(capname).unwrap();
        let ret   = ncurses::ll::tigetstr(cap_c.as_ptr());

        // See the man page for tigetstr(3).
        if ret.is_null() {
            None
        }
        else if ret == (-1 as isize as *mut c_char) {
            None
        }
        else {
            let bytes = CStr::from_ptr(ret).to_bytes();
            Some(String::from_utf8_unchecked(bytes.to_vec()))
        }
    }
}
