use color_space::{CompareCie2000, ToRgb};
use crate::color::{ANSIColor, Color, RGBColor};
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

type Palette = LruCache<RGBColor, PaletteIndex>;
type Pairs   = LruCache<Pair, PairIndex>;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
struct Pair {
    fg: PaletteIndex,
    bg: PaletteIndex
}

impl Default for Pair {
    fn default() -> Self {
        Self {
            fg: -1,
            bg: -1
        }
    }
}

/// In order to efficiently use color palettes, we search for a
/// sufficently similar color in a palette before defining a new
/// color. This constant is a threshold of the CIE2000 distance
/// between two RGB colors.
///
/// But LOL, I don't know what the fuck is the appropriate value for
/// this. This is just arbitrary right now, and will hopefully be
/// adjusted some day.
const SIMILARITY_THRESHOLD: f64 = 2.3;

#[derive(Debug)]
enum TermType {
    NoColors,
    PalettedColor {
        pairs: Pairs,
        palette: Palette,
        is_mutable: bool
    } ,
    DirectColor {
        /// 'None' represents the default color.
        pairs: Pairs,
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
                        pairs: Pairs::new(ncurses::COLOR_PAIRS().try_into().unwrap()),
                        r_bits,
                        g_bits,
                        b_bits
                    }
                }
                else {
                    let pairs = Pairs::new(ncurses::COLOR_PAIRS().try_into().unwrap());
                    let mut palette = Palette::new(ncurses::COLORS().try_into().unwrap());
                    let is_mutable = ncurses::can_change_color();

                    if is_mutable {
                        TermType::PalettedColor {
                            pairs,
                            palette,
                            is_mutable
                        }
                    }
                    else {
                        // The terminal doesn't allow mutating the
                        // palette but we have no idea what colors it
                        // has. So we assume it has the standard ANSI
                        // colors.
                        if palette.cap() >= 8 {
                            populate_ansi_palette(&mut palette);
                            TermType::PalettedColor {
                                pairs,
                                palette,
                                is_mutable
                            }
                        }
                        else {
                            // But it even doesn't have 8 colors. What
                            // the fuck does it have?
                            TermType::NoColors
                        }
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

    pub fn set_colors(&mut self, w: ncurses::WINDOW, fg_color: impl Color, bg_color: impl Color) {
        let fallback_fg = self.fallback_default_fg_to();
        let fallback_bg = self.fallback_default_bg_to();

        match self.ttype {
            TermType::NoColors => {
                // Can't do anything about that.
            },
            TermType::PalettedColor {
                ref mut pairs,
                ref mut palette,
                is_mutable
            } => {
                let pair = {
                    if is_mutable {
                        Pair {
                            fg: find_or_create_color(fallback_fg, palette, &fg_color),
                            bg: find_or_create_color(fallback_bg, palette, &bg_color)
                        }
                    }
                    else {
                        Pair {
                            fg: find_closest_color(fallback_fg, palette, &fg_color),
                            bg: find_closest_color(fallback_bg, palette, &bg_color)
                        }
                    }
                };
                let pair_idx = find_or_create_pair(pairs, pair);
                check(safe_wcolor_set(w, pair_idx)).unwrap();
            },
            TermType::DirectColor {
                ref mut pairs,
                r_bits,
                g_bits,
                b_bits
            } => {
                let pair =
                    Pair {
                        fg: pack_color(fallback_fg, r_bits, g_bits, b_bits, &fg_color),
                        bg: pack_color(fallback_bg, r_bits, g_bits, b_bits, &bg_color)
                    };
                let pair_idx = find_or_create_pair(pairs, pair);
                check(safe_wcolor_set(w, pair_idx)).unwrap();
            }
        }
    }

    fn fallback_default_fg_to(&self) -> Option<RGBColor> {
        if self.has_default_colors {
            None
        }
        else {
            Some(ANSIColor::White.as_rgb())
        }
    }

    fn fallback_default_bg_to(&self) -> Option<RGBColor> {
        if self.has_default_colors {
            None
        }
        else {
            Some(ANSIColor::Black.as_rgb())
        }
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

fn populate_ansi_palette(palette: &mut Palette) {
    assert!(palette.cap() >= 8);
    assert!(palette.len() == 0);

    palette.put(ANSIColor::Black  .as_rgb(), 0);
    palette.put(ANSIColor::Red    .as_rgb(), 1);
    palette.put(ANSIColor::Green  .as_rgb(), 2);
    palette.put(ANSIColor::Yellow .as_rgb(), 3);
    palette.put(ANSIColor::Blue   .as_rgb(), 4);
    palette.put(ANSIColor::Magenta.as_rgb(), 5);
    palette.put(ANSIColor::Cyan   .as_rgb(), 6);
    palette.put(ANSIColor::White  .as_rgb(), 7);
}

/// This assumes the palette is mutable on this terminal.
fn find_or_create_color(fallback_default_to: Option<RGBColor>,
                        palette: &mut Palette,
                        color: &impl Color)
                        -> PaletteIndex {
    if let Some(similar) = find_similar_color(fallback_default_to, palette, color) {
        similar
    }
    else {
        let rgb = color.as_rgb();
        let idx =
            if palette.len() < palette.cap() {
                // There is still a room for a new color. Create one.
                let idx = TryInto::<PaletteIndex>::try_into(palette.len()).unwrap();
                palette.put(rgb, idx);
                idx
            }
            else {
                // No more room for a new color. Reuse the least
                // recently used one, hoping that the color is not not
                // used currently.
                let idx = palette.pop_lru().unwrap().1;
                palette.put(rgb, idx);
                idx
                // Maybe we should remove pairs that using this color,
                // but we don't because the fact this color isn't used
                // for a long time implies those pairs are also not
                // used recently.
            };

        #[cfg(feature = "extended-colors")]
        fn scale(c: u8) -> i32 {
            (c as i32 * 1000) / 255
        }

        #[cfg(feature = "extended-colors")]
        check(ncurses::init_extended_color(idx, scale(rgb.r), scale(rgb.g), scale(rgb.b))).unwrap();

        #[cfg(not(feature = "extended-colors"))]
        fn scale(c: u8) -> i16 {
            (c as i16 * 1000) / 255
        }

        #[cfg(not(feature = "extended-colors"))]
        check(ncurses::init_color(idx, scale(rgb.r), scale(rgb.g), scale(rgb.b))).unwrap();

        idx
    }
}

fn find_similar_color(fallback_default_to: Option<RGBColor>,
                      palette: &mut Palette,
                      color: &impl Color)
                      -> Option<PaletteIndex> {
    // If it's DefaultColor it's a special case.
    if let Some(-1) = color.magic_index() {
        if let Some(rgb) = fallback_default_to {
            // The terminal has no default colors.
            find_similar_rgb(palette, rgb)
        }
        else {
            // The magic number representing the default color.
            Some(-1)
        }
    }
    else {
        find_similar_rgb(palette, color.as_rgb())
    }
}

fn find_similar_rgb(palette: &mut Palette, color: RGBColor) -> Option<PaletteIndex> {
    // First try an exact match. If we already have exactly the same
    // color in the palette, we can simply use it.
    if let idx@Some(_) = palette.get(&color) {
        // Found.
        idx.cloned()
    }
    else {
        // Then search for a sufficiently similar color in the
        // palette. By "sufficiently similar" we mean some color that
        // is mostly indistinguishable by the human eye.
        let mut found: Option<(RGBColor, PaletteIndex)> = None;
        for (rgb, idx) in palette.iter() {
            // THINKME: Maybe this is awfully slow?
            let distance = color.to_rgb().compare_cie2000(&rgb.to_rgb());

            if distance <= SIMILARITY_THRESHOLD {
                // This one is good enough. Mark it as the most
                // recently used, and use it.
                found = Some((*rgb, *idx));
                break;
            }
        }
        found.map(|(rgb, idx)| {
            palette.get(&rgb);
            idx
        })
    }
}

/// This assumes the palette is immutable on this terminal.
fn find_closest_color(fallback_default_to: Option<RGBColor>,
                      palette: &Palette,
                      color: &impl Color)
                      -> PaletteIndex {
    // If it's DefaultColor it's a special case.
    if let Some(-1) = color.magic_index() {
        if let Some(rgb) = fallback_default_to {
            // The terminal has no default colors.
            find_closest_rgb(palette, rgb)
        }
        else {
            // The magic number representing the default color.
            -1
        }
    }
    else {
        find_closest_rgb(palette, color.as_rgb())
    }
}

fn find_closest_rgb(palette: &Palette, color: RGBColor) -> PaletteIndex {
    let mut closest: Option<(f64, PaletteIndex)> = None;
    for (rgb, idx) in palette.iter() {
        // THINKME: Maybe this is awfully slow?
        let distance = color.to_rgb().compare_cie2000(&rgb.to_rgb());

        if let Some((closest_dist, _)) = closest {
            if distance < closest_dist {
                closest = Some((distance, *idx));
            }
        }
        else {
            closest = Some((distance, *idx));
        }
    }
    closest.expect("The color palette is expected to be non-empty.").1
}

fn pack_color(fallback_default_to: Option<RGBColor>,
              r_bits: u8, g_bits: u8, b_bits: u8,
              color: &impl Color)
              -> PaletteIndex {

    let max_r = (1 << (r_bits as PaletteIndex)) - 1;
    let max_g = (1 << (g_bits as PaletteIndex)) - 1;
    let max_b = (1 << (b_bits as PaletteIndex)) - 1;
    let pack = |rgb: RGBColor| -> PaletteIndex {
        let c_r = (rgb.r as PaletteIndex * max_r) / 255;
        let c_g = (rgb.g as PaletteIndex * max_g) / 255;
        let c_b = (rgb.b as PaletteIndex * max_b) / 255;

        (c_r & r_bits as PaletteIndex) << (g_bits + b_bits) |
        (c_g & g_bits as PaletteIndex) << b_bits |
        (c_b & b_bits as PaletteIndex)
    };

    // If it's DefaultColor it's a special case.
    if let Some(-1) = color.magic_index() {
        if let Some(rgb) = fallback_default_to {
            // The terminal has no default colors.
            pack(rgb)
        }
        else {
            // The magic number representing the default color.
            -1
        }
    }
    else {
        pack(color.as_rgb())
    }
}

fn find_or_create_pair(pairs: &mut Pairs, pair: Pair) -> PairIndex {
    if pair == Pair::default() {
        // The pair index 0 is special. It means the default
        // foreground and background, and curses doesn't allow
        // applications to change it. This means we will have at most
        // pairs.cap()-1 pairs.
        0
    }
    else if let Some(idx) = pairs.get(&pair) {
        // Found an index in the pairs. Use it.
        *idx
    }
    else {
        let idx =
            if pairs.len() < pairs.cap() - 1 {
                // There is still a room for a new pair. Create one.
                let idx = TryInto::<PairIndex>::try_into(pairs.len()).unwrap() + 1;
                pairs.put(pair, idx);
                idx
            }
            else {
                // No more room for a new pair. Reuse the least
                // recently used one, hoping that the pair is not used
                // currently.
                let idx = pairs.pop_lru().unwrap().1;
                pairs.put(pair, idx);
                idx
            };

        #[cfg(feature = "extended-colors")]
        check(ncurses::init_extended_pair(idx, pair.fg, pair.bg)).unwrap();

        #[cfg(not(feature = "extended-colors"))]
        check(ncurses::init_pair(idx, pair.fg, pair.bg)).unwrap();

        idx
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

// ncurses::wcolor_set() doesn't support extended colors.
fn safe_wcolor_set(w: ncurses::WINDOW, pair: PairIndex) -> i32 {
    unsafe {
        let p16  = pair as i16;
        let opts = &pair as *const i32 as ncurses::ll::void_p;

        ncurses::ll::wcolor_set(w, p16, opts)
    }
}
