#![feature(proc_macro, wasm_custom_section, wasm_import_module)]

extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern {
    #[wasm_bindgen(js_namespace = console)]
    fn time(name: &str);

    #[wasm_bindgen(js_namespace = console)]
    fn timeEnd(name: &str);
}

pub struct Timer<'a> {
    name: &'a str,
}

impl<'a> Timer<'a> {
    pub fn new(name: &'a str) -> Timer<'a> {
        time(name);
        Timer { name }
    }
}

impl<'a> Drop for Timer<'a> {
    fn drop(&mut self) {
        timeEnd(self.name);
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cell {
    Dead  = 0,
    Alive = 1,
}

impl Cell {
    fn toggle(&mut self) {
        *self = match *self {
            Cell::Dead => Cell::Alive,
            Cell::Alive => Cell::Dead,
        };
    }
}

#[wasm_bindgen]
pub struct Universe {
    width:  u32,
    height: u32,
    cells: Vec<Cell>,
}

/// Public methods, exported to JavaScript.
#[wasm_bindgen]
impl Universe {
    pub fn new(w:u32, h:u32) -> Universe {
        let width  = w;
        let height = h;
        let cells  = (0..height * width).map(|i| {
            if i < width || i % width == 0 || i % width == width-1 || width * (height-1) <= i
                { Cell::Alive} else {Cell::Dead}
        }).collect();
        Universe {
            width,
            height,
            cells,
        }
    }

    pub fn width(&self)  -> u32 { self.width  }
    pub fn height(&self) -> u32 { self.height }
    pub fn cells(&self) -> *const Cell {
        self.cells.as_ptr()
    }

    pub fn tick(&mut self) {
        // let _timer = Timer::new("Universe::tick");
        let mut next = self.cells.clone();

        for x in 0..self.width {
            for y in 0..self.height {
                let k = 3 * self.lvl1_neighbour_count(x, y)
                      +     self.lvl2_neighbour_count(x, y);
                next[self.get_index(x, y)] =
                    match self.get(x,y) {
                      Cell::Alive => if 6 <= k && k <= 10 { Cell::Alive } else { Cell::Dead },
                      Cell::Dead  => if 7 <= k && k <=  9 { Cell::Alive } else { Cell::Dead },
                    };
            }
        }
        self.cells = next;
    }
}

/// Private methods.
#[wasm_bindgen]
impl Universe {
    fn get_index(&self, x: u32, y: u32) -> usize {
        let x_ = if x < self.width  { x } else { x - self.width  };
        let y_ = if y < self.height { y } else { y - self.height };
        (x_ + y_ * self.width) as usize
    }

    pub fn toggle_cell(&mut self, x: u32, y: u32) {
        let idx = self.get_index(x, y);
        self.cells[idx].toggle();
    }

    fn get(&self, x: u32, y: u32) -> Cell {
        self.cells[self.get_index(x,y)]
    }

    fn lvl1_neighbour_count(&self, x: u32, y: u32) -> u8 {
        let xx = x+self.width;
        let yy = y+self.height;
        [    (x,y+1),(x+1,y+1)
        ,(xx-1,y)/*(x,y)*/,(x+1,y)
        ,    (x,yy-1),(x+1,yy-1)
        ].iter().map(|(x_,y_)| {
            self.get(*x_, *y_) as u8
        }).sum()
    }

    fn lvl2_neighbour_count(&self, x: u32, y: u32) -> u8 {
        let xx = x+self.width;
        let yy = y+self.height;
        [          (x,y+2),
         (xx-1,y+1),        (x+2,y+1),
                  /*(x,y)*/
         (xx-1,yy-1),        (x+2,yy-1),
                   (x,yy-2)
        ].iter()
         .map(|(x_,y_)| {
            self.get(*x_, *y_) as u8
        }).sum()
    }
}
