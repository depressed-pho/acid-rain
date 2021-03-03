use ctk::{
    Border,
    Component,
    Graphics,
    RootWindow
};
use ctk::border::NullBorder;
use ctk::color::DefaultColor;
use ctk::dimension::{
    Point,
    Rectangle,
    SizeRequirements
};
use ctk::graphics::Attribute;
use num::Zero;
use rain_core::world::World;
use std::sync::{Arc, RwLock};
use uuid::Uuid;

#[derive(Debug)]
pub struct WorldView<W: World> {
    graphics: Graphics,
    bounds: Rectangle,
    border: Box<dyn Border>,
    dirty: bool,

    world: Arc<RwLock<W>>,
    /// The player to track.
    player: Uuid,
    /// The offset from the center of the component where the player
    /// should be located.
    player_offset: Point
}

impl<W: World> WorldView<W> {
    pub fn new(world: Arc<RwLock<W>>, player: Uuid) -> Self {
        Self {
            graphics: Graphics::new(),
            bounds: Rectangle::default(),
            border: Box::new(NullBorder::default()),
            dirty: true,
            world,
            player,
            player_offset: Point::zero()
        }
    }

    fn draw_tiles(&mut self) {
        let inner = self.get_inner();

        // FIXME
        use ctk::color::RGBColor;
        self.graphics.set_fg(RGBColor {r: 255, g: 64, b: 64});
        self.graphics.draw_char('.', inner.pos);
    }

    fn draw_player(&mut self) {
        let inner  = self.get_inner();
        let center = Point {
            x: inner.pos.x + (inner.size.width - 1) / 2,
            y: inner.pos.y + (inner.size.height - 1) / 2
        };
        self.graphics.attr_on(Attribute::Bold.into());
        self.graphics.set_colors(DefaultColor(), DefaultColor());
        self.graphics.draw_char('@', center + self.player_offset);
    }
}

impl<W: World> Component for WorldView<W> {
    fn paint(&mut self) {
        if self.dirty {
            self.border.paint(&mut self.graphics);

            let inner = self.get_inner();
            self.graphics.clear_rect(inner);

            self.draw_tiles();
            self.draw_player();
        }
        self.dirty = true;
    }

    fn refresh(&self, root: &RootWindow, offset: Point) {
        self.graphics.refresh(root, self.get_location() + offset);
    }

    fn get_bounds(&self) -> Rectangle {
        self.bounds
    }

    fn set_bounds(&mut self, b: Rectangle) {
        self.bounds = b;
        if self.graphics.set_size(b.size) {
            self.dirty = true;
        }
    }

    fn get_size_requirements(&self) -> SizeRequirements {
        SizeRequirements::any()
    }

    fn get_border(&self) -> &Box<dyn Border> {
        &self.border
    }

    fn set_border(&mut self, b: Box<dyn Border>) {
        self.border = b;
    }
}
