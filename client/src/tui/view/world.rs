use ctk::{
    Border,
    Component,
    Graphics,
    RootWindow,
    is_utf8_locale
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
use rain_core::world::chunk::{ChunkPos, TileOffset};
use std::cmp::{max, min};
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

    /// Get the center of the component in the component coordinates.
    fn get_center(&self) -> Point {
        let inner = self.get_inner();
        Point {
            x: inner.pos.x + (inner.size.width - 1) / 2,
            y: inner.pos.y + (inner.size.height - 1) / 2
        }
    }

    /// Convert a point in the component coords to that of the world coords.
    fn world_pos_at(&self, cp: Point) -> WorldPos {
        // Get the player position in the world coords.
        let world  = self.world.read().unwrap();
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

    fn draw_chunks(&mut self) {
        let inner = self.get_inner();

        /* Draw all the tiles currently visible from the
         * viewpoint. The easiest way to do this is to iterate on
         * every visible world position and ask the world for the tile
         * there, but that would cause too many chunk lookups and
         * would be terribly inefficient. So we iterate on visible
         * chunks instead, and render their visible parts.
         */
        let w_top_left    = self.world_pos_at(inner.pos);
        let w_top_right   = w_top_left.map_x(|x| x + inner.size.width - 1);
        let w_bottom_left = w_top_left.map_y(|y| y + inner.size.height - 1);
        let c_top_left    = ChunkPos::from(w_top_left);
        let c_top_right   = ChunkPos::from(w_top_right);
        let c_bottom_left = ChunkPos::from(w_bottom_left);

        // We'll use this to later translate a WorldPos to component
        // coords.
        let delta = Dimension {
            width:  inner.pos.x - w_top_left.x,
            height: inner.pos.y - w_top_left.y
        };
        let component_pos_at = |wpos: WorldPos| -> Point {
            Point {
                x: wpos.x + delta.width,
                y: wpos.y + delta.height
            }
        };

        let mut world = self.world.write().unwrap();

        for cy in c_top_left.y ..= c_bottom_left.y {
            for cx in c_top_left.x ..= c_top_right.x {
                let cpos = ChunkPos { x: cx, y: cy };
                world.ensure_chunk_exists(cpos);
                if let Some(chunk) = world.get_chunk(cpos) {
                    /* Now the problem is how to determine the visible
                     * area of this chunk. For each chunk we know
                     * which area in the world coords the chunk
                     * covers.
                     */
                    let wc_top_left    = cpos.into_wpos(0);
                    let wc_top_right   = ChunkPos::from(wc_top_left).map_x(|x| x + 1).into_wpos(0).map_x(|x| x - 1);
                    let wc_bottom_left = ChunkPos::from(wc_top_left).map_y(|y| y + 1).into_wpos(0).map_y(|y| y - 1);

                    /* And we know which area in the world coords is
                     * visible.
                     */
                    for wy in max(w_top_left.y, wc_top_left.y) ..= min(w_bottom_left.y, wc_bottom_left.y) {
                        for wx in max(w_top_left.x, wc_top_left.x) ..= min(w_top_right.x, wc_top_right.x) {
                            let wpos = WorldPos { x: wx, y: wy, z: 0 };
                            let off  = TileOffset::from(wpos);
                            let ts   = chunk.get_tile_state(&off);
                            let appr = ts.appearance();

                            self.graphics
                                .attr_set(appr.attrs())
                                .set_colors(appr.fg_color(), appr.bg_color());

                            if is_utf8_locale() {
                                self.graphics.draw_string(appr.unicode(), component_pos_at(wpos));
                            }
                            else {
                                self.graphics.draw_char(appr.ascii(), component_pos_at(wpos));
                            }
                        }
                    }
                }
            }
        }
    }

    fn draw_player(&mut self) {
        let center = self.get_center();
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

            self.draw_chunks();
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
