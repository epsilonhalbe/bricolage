import { Universe } from "./wasm_game_of_life.js";
import { memory }   from "./wasm_game_of_life_bg.wasm";

const CELL_SIZE = 20;

//const GRID_COLOR = "#FFAA00";
const COLOR_DEAD = "#111111";
// These must match `Cell::Alive` and `Cell::Dead` in `src/lib.rs`.
const DEAD  = 0;
const ALIVE = 1;

const w_ = 800;
const h_ = 600;

const w = Math.ceil(w_/(CELL_SIZE * 1.5 * 1.1) + 1)/2;
const h = Math.ceil(h_/CELL_SIZE + 0.8660254037844387 * 1.1);

const universe = Universe.new(w,h);
const width  = universe.width();
const height = universe.height();

// Initialize the canvas with room for all of our cells and a 1px border
// around each of them.
const canvas = document.getElementById("hex-of-life-canvas");
canvas.width  = CELL_SIZE * (2 * width - 1) * 1.5 * 1.1;
canvas.height = CELL_SIZE * (height - 0.8660254037844387 * 1.1);

const ctx = canvas.getContext('2d');

let animationId = null;

const fps = new class {
  constructor() {
    this.fps = document.getElementById("fps");
    this.frames = [];
    this.lastFrameTimeStamp = performance.now();
  }

  render() {
    const now = performance.now();
    const delta = now - this.lastFrameTimeStamp;
    this.lastFrameTimeStamp = now;
    const fps = 1 / delta * 1000;

    this.frames.push(fps);
    if (this.frames.length > 100) {
      this.frames.shift();
    }

    let min = Infinity;
    let max = -Infinity;
    let sum = 0;
    for (let i = 0; i < this.frames.length; i++) {
      sum += this.frames[i];
      min = Math.min(this.frames[i], min);
      max = Math.max(this.frames[i], max);
    }
    let mean = sum / this.frames.length;

    this.fps.textContent = `
Frames per Second:
         latest = ${Math.round(fps)}
avg of last 100 = ${Math.round(mean)}
min of last 100 = ${Math.round(min)}
max of last 100 = ${Math.round(max)}
`.trim();
  }
};

var slow = -1;
const fadeRate = 80;

const renderLoop = () => {
  fps.render();
  if (fadeRate <= ++slow) {
    //for (let i = 0; i < 1; i++) {
      slow = 0;
      universe.tick();
    //}
  }
  drawCells(slow);
  animationId = requestAnimationFrame(renderLoop);
};

const getIndex = (x, y) => {
  return x + y * width;
};

const drawHexagon = (x,y) => {
    const x0 = 3.3*(x+(y%2)/2)*CELL_SIZE;
    const y0 = y*CELL_SIZE;
    ctx.beginPath();
    ctx.moveTo(x0+CELL_SIZE,y0);
    // pre-calculated hex_agon angles -for performance
    const y1 = CELL_SIZE*0.8660254037844386;
    const x1 = CELL_SIZE*0.5;
    const x2 = CELL_SIZE;
    ctx.lineTo(x0+x1,y0+y1);
    ctx.lineTo(x0-x1,y0+y1);
    ctx.lineTo(x0-x2,y0);
    ctx.lineTo(x0-x1,y0-y1);
    ctx.lineTo(x0+x1,y0-y1);
    ctx.lineTo(x0+x2,y0);
    ctx.closePath();
    ctx.fill();
}

const drawCell = (what) => {
  for (var y = 0; y < height; y++) {
    for (var x = 0; x < width; x++) {
      const idx = getIndex(x,y);
      if (cells[idx] !== what) {
        continue;
      }
      drawHexagon(x,y);
    }
  }
};

const drawCells = (n) => {
  const cellsPtr = universe.cells();
  const cells = new Uint8Array(memory.buffer, cellsPtr, width * height);

  // Because changing the `fillStyle` is an expensive operation, we want to
  // avoid doing it for every cell. Instead, we do two passes: one for live
  // cells, and one for dead cells.

  ctx.clearRect(0, 0, canvas.width, canvas.height);
  // Dead cells.
  ctx.fillStyle = COLOR_DEAD;
  for (var y = 0; y < height; y++) {
    for (var x = 0; x < width; x++) {
      const idx = getIndex(x,y);
      if (cells[idx] !== DEAD) {
        continue;
      }
      drawHexagon(x,y);
    }
  }

  // Live cells.
  const t_ = (n*n)/(fadeRate*fadeRate);
  const t  = boundedBy(0, n <= fadeRate/2 ? 4*t_ : 1-t_, 0.5);
  ctx.fillStyle = rgb(17+t*(226-17),17+t*(0-17),17+t*(116-17));
  for (var y = 0; y < height; y++) {
    for (var x = 0; x < width; x++) {
      const idx = getIndex(x,y);
      if (cells[idx] !== ALIVE) {
        continue;
      }
      drawHexagon(x,y);
    }
  }
};

const playPauseButton = document.getElementById("play-pause");

const isPaused = () => {
  return animationId === null;
};

const play = () => {
  playPauseButton.textContent = "⏸";
  renderLoop();
};

const pause = () => {
  playPauseButton.textContent = "▶";
  cancelAnimationFrame(animationId);
  animationId = null;
};

playPauseButton.addEventListener("click", event => {
  if (isPaused()) { play(); } else { pause(); }
});

const boundedBy = (a,x,b) => Math.min(Math.max(a,x),b);
const rgb = (r,g,b) => "rgb("+r+","+g+","+b+")";

canvas.addEventListener("click", event => {
  const boundingRect = canvas.getBoundingClientRect();

  const mouseX = event.clientX - boundingRect.left;
  const mouseY = event.clientY - boundingRect.top ;
  var x  = (mouseX / CELL_SIZE / 1.65);
  var y  = (mouseY / CELL_SIZE);
  const shape = [Math.floor(x)%2
                ,Math.floor(y)%2
                ,0 + (1/3 + (1-(y%1))/3.15 < (x % 1))] ;
  var x  = Math.floor(mouseX / CELL_SIZE / 3.3);
  var y  = 2*Math.floor(mouseY / CELL_SIZE/2)+1;
  switch (shape) {
    case [0,0,0]:
          y--;
          break;
    case [1,0,1]:
          x++;
          y--;
          break;
    case [0,1,0]:
          y++;
          break;
    case [1,1,1]:
          x++;
          y++;
          break;
  }

  universe.toggle_cell(x, y);
  drawCells(slow);
});

play();
pause();
