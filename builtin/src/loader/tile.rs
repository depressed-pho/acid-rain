use color_space::Hsl;
use rain_core::tui::Appearance;
use rain_core::world::tile::{Tile, TileRegistry, TileStateValue};
use std::sync::Arc;

#[derive(Debug)]
struct Dirt {
    apr: Appearance
}
impl Default for Dirt {
    fn default() -> Self {
        Dirt {
            apr: Appearance::builder()
                .unicode(".")
                .ascii('.')
                .fg_color(Hsl::new(25.0, 0.75, 0.47)) // CSS3 chocolate
                .build()
        }
    }
}
impl Tile for Dirt {
    fn id(&self) -> &str {
        return "acid-rain:dirt";
    }
    fn appearance(&self, _: TileStateValue) -> &Appearance {
        &self.apr
    }
}

pub fn load_tiles(reg: &mut TileRegistry) {
    reg.register(Arc::new(Dirt::default())).unwrap();
}
