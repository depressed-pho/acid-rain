use async_trait::async_trait;
use ctk::{
    Border,
    Component,
    Graphics,
    RootWindow
};
use ctk::attribute::Attribute;
use ctk::border::NullBorder;
use ctk::color::DefaultColor;
use ctk::dimension::{
    Dimension,
    Point,
    Rectangle,
    SizeRequirements
};
use num::Zero;
use rain_core::world::World;
use rain_core::world::position::WorldPos;
use rain_core::world::chunk::ChunkPos;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

#[derive(Debug)]
pub struct WorldView<W: World + Send + Sync> {
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

impl<W: World + Send + Sync> WorldView<W> {
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

    /// Get the center of the component in the component coordinates.
    fn get_center(&self) -> Point {
        let inner = self.get_inner();
        Point {
            x: inner.pos.x + (inner.size.width - 1) / 2,
            y: inner.pos.y + (inner.size.height - 1) / 2
        }
    }

    /// Convert a point in the component coords to that of the world coords.
    async fn world_pos_at(&self, cp: Point) -> WorldPos {
        // Get the player position in the world coords.
        let world  = self.world.read().await;
        let player = world.get_player(&self.player).expect("Player is missing");
        let ppos_w = player.get_pos();

        // Get the player position in the component coords.
        let center = self.get_center();
        let ppos_l = center + self.player_offset;

        // Now that we have these, we know how these coords correspond
        // to each other.
        let delta  = Dimension {
            width:  ppos_w.x - ppos_l.x,
            height: ppos_w.y - ppos_l.y
        };
        WorldPos {
            x: cp.x + delta.width,
            y: cp.y + delta.height,
            z: ppos_w.z
        }
    }

    async fn draw_chunks(&mut self) {
        let inner = self.get_inner();

        /* Draw all the tiles currently visible from the
         * viewpoint. The easiest way to do this to iterate on every
         * visible position and ask the world for the tile there, but
         * that would cause too many atomic operations and would be
         * terribly inefficient. The second easiest approach is to
         * collect all the visible chunks and read-lock them all, then
         * iterate on positions, but this may cause a dead lock. So we
         * iterate on visible chunks instead, and render their visible
         * parts with locking only one chunk at a time. This may
         * render chunks inconsistently, but that should be
         * acceptable.
         */
        let w_top_left     = self.world_pos_at(inner.pos).await;
        let w_top_right    = w_top_left. map_x(|x| x + inner.size.width);
        let w_bottom_left  = w_top_left. map_y(|y| y + inner.size.height);
        let c_top_left     = ChunkPos::from(w_top_left);
        let c_top_right    = ChunkPos::from(w_top_right);
        let c_bottom_left  = ChunkPos::from(w_bottom_left);

        let world = self.world.read().await;
        let cm    = world.get_chunk_manager();

        for y in c_top_left.y .. c_bottom_left.y {
            for x in c_top_left.x .. c_top_right.y {
                let cpos   = ChunkPos { x, y };
                let chunk_ = cm.get(cpos).await;
                let chunk  = chunk_.read().await;
                //panic!("{:?}", chunk);
            }
        }

        // FIXME
        use ctk::color::RGBColor;
        self.graphics.set_fg(RGBColor {r: 255, g: 64, b: 0});
        self.graphics.draw_char('#', inner.pos);
    }

    fn draw_player(&mut self) {
        let center = self.get_center();
        self.graphics.attr_on(Attribute::Bold.into());
        self.graphics.set_colors(DefaultColor(), DefaultColor());
        self.graphics.draw_char('@', center + self.player_offset);
    }
}

#[async_trait]
impl<W: World + Send + Sync> Component for WorldView<W> {
    async fn paint(&mut self) {
        if self.dirty {
            self.border.paint(&mut self.graphics).await;

            let inner = self.get_inner();
            self.graphics.clear_rect(inner);

            self.draw_chunks().await;
            self.draw_player();
        }
        self.dirty = true;
    }

    async unsafe fn refresh(&self, root: &RootWindow, offset: Point) {
        self.graphics.refresh(root, self.get_location() + offset);
    }

    fn get_bounds(&self) -> Rectangle {
        self.bounds
    }

    async fn set_bounds(&mut self, b: Rectangle) {
        self.bounds = b;
        if self.graphics.set_size(b.size) {
            self.dirty = true;
        }
    }

    async fn get_size_requirements(&self) -> SizeRequirements {
        SizeRequirements::any()
    }

    fn get_border(&self) -> &Box<dyn Border> {
        &self.border
    }

    fn set_border(&mut self, b: Box<dyn Border>) {
        self.border = b;
    }
}
