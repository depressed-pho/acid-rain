use crate::util::check;

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
    has_colors: bool,
    has_default_colors: bool
}

impl ColorManager {
    /// Construct a color manager. This has to be done in the same
    /// thread which have instantiated Ctk.
    pub fn new() -> Result<Self, ()> {
        let has_colors = ncurses::has_colors();
        let has_default_colors = {
            if has_colors {
                /* We are going to use colors. At first we define no color
                 * pairs, and progressively define them when needed. */
                check(ncurses::start_color())?;

                /* Applications may want to use default_colors(3). See
                 * if it's supported by the terminal. */
                ncurses::use_default_colors() == ncurses::OK
            }
            else {
                false
            }
        };

        Ok(Self {
            has_colors,
            has_default_colors
        })
    }
}
