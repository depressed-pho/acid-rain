use crate::{
    Graphics,
    Symbol,
    is_utf8_locale
};
use crate::border::Border;
use crate::dimension::{
    Dimension,
    Insets,
    Point
};
use num::Zero;

#[derive(Debug)]
pub struct ButtonBorder {}

impl Default for ButtonBorder {
    fn default() -> Self {
        ButtonBorder {}
    }
}

impl Border for ButtonBorder {
    fn get_insets(&self) -> Insets {
        Insets {
            left: 1,
            right: 1,
            top: 0,
            bottom: 0
        }
    }

    fn paint(&self, g: &mut Graphics) {
        /* For possible other candidates for symbols see
         * https://en.wikipedia.org/wiki/Bracket
         */
        match g.get_size() {
            Dimension { width: 1, height: 1 } => {
                /* Special case for buttons which is only 1x1.
                 */
                g.draw_symbol(Symbol::Block, Point::zero());
            },
            Dimension { width: w, height: 1 } => {
                /* Special case for buttons whose height is 1, which
                 * is actually the most usual case.
                 */
                let l_sym = "["; // FIXME: Try U+27E6 and U+27E7.
                let r_sym = "]";
                g.draw_string(l_sym, Point::zero());
                g.draw_string(r_sym, Point { x: w-1, y: 0 });
            },
            Dimension { width: w, height: h } => {
                if is_utf8_locale() {
                    let l_u_sym = "⎡"; // U+23A1 LEFT SQUARE BRACKET UPPER CORNER
                    let l_e_sym = "⎢"; // U+23A2 LEFT SQUARE BRACKET EXTENSION
                    let l_l_sym = "⎣"; // U+23A3 LEFT SQUARE BRACKET LOWER CORNER
                    let r_u_sym = "⎤"; // U+23A4 RIGHT SQUARE BRACKET UPPER CORNER
                    let r_e_sym = "⎥"; // U+23A5 RIGHT SQUARE BRACKET EXTENSION
                    let r_l_sym = "⎦"; // U+23A6 RIGHT SQUARE BRACKET LOWER CORNER
                    g.draw_string(l_u_sym, Point::zero());
                    g.draw_string(r_u_sym, Point { x: w-1, y: 0 });
                    for y in 1..h-1 {
                        g.draw_string(l_e_sym, Point { x: 0,   y });
                        g.draw_string(r_e_sym, Point { x: w-1, y });
                    }
                    g.draw_string(l_l_sym, Point { x: 0,   y: h-1 });
                    g.draw_string(r_l_sym, Point { x: w-1, y: h-1 });
                }
                else {
                    g.draw_symbol(Symbol::UpperLeftCorner,  Point::zero());
                    g.draw_symbol(Symbol::UpperRightCorner, Point { x: w-1, y: 0 });
                    for y in 1..h-1 {
                        g.draw_symbol(Symbol::VerticalLine, Point { x: 0,   y });
                        g.draw_symbol(Symbol::VerticalLine, Point { x: w-1, y });
                    }
                    g.draw_symbol(Symbol::LowerLeftCorner,  Point { x: 0,   y: h-1 });
                    g.draw_symbol(Symbol::LowerRightCorner, Point { x: w-1, y: h-1 });
                }
            }
        }
    }
}
