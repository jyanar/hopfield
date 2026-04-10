# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Hopfield is an interactive browser-based Hopfield neural network simulator written in [Elm](https://elm-lang.org/) (v0.19.1). It provides a resizable grid (10×10, 16×16, or 28×28) where users can draw patterns, store them as memories using Hebbian learning, and observe the network recall stored patterns through asynchronous or synchronous updates. The live app is deployed at https://yanar.org/hopfield.

## Repository Structure

This is a single-file Elm application. All logic — model, update, view, and helper functions — lives in `src/Main.elm`. The HTML shell is `index.html`.

## Build & Run

```sh
# Build (produces main.js loaded by index.html)
elm make src/Main.elm --output=main.js --optimize

# Development server (visit http://localhost:8000/src/Main.elm)
elm reactor
```

After building, open `index.html` directly in a browser — no dev server required.

## Testing & Formatting

No test framework or linter is configured. Standard tools: [elm-test](https://github.com/elm-explorations/test) and [elm-format](https://github.com/avh4/elm-format).

## CI/CD

GitHub Actions (`.github/workflows/elm-to-gh-pages.yml`) triggers on push to `main`: builds with Elm 0.19.1, deploys to GitHub Pages. One deployment at a time.

## Architecture

The app follows **The Elm Architecture (TEA)**:

### Model (`src/Main.elm:59-67`)

```elm
type alias Model =
    { state    : List UnitState        -- N neurons, each On or Off
    , weights  : List (List Int)       -- N×N weight matrix
    , mems     : List (List UnitState) -- stored memory patterns
    , playing  : Bool                  -- simulation auto-running
    , drawing  : Bool                  -- user dragging to draw
    , drawMode : DrawMode              -- Paint (turn On) or Erase (turn Off)
    , gridSize : GridSize              -- current grid dimensions {rows, cols}
    }
```

### Key Types

- `UnitState` — `On | Off` enum for neuron activation
- `DrawMode` — `Paint | Erase` controls whether mouse drag turns cells on or off
- `GridSize` — `{ rows : Int, cols : Int }`; predefined sizes: `small` (10×10), `medium` (16×16), `large` (28×28)
- `Msg` — union type with all user interactions and system events (`src/Main.elm:87`)

### Core Algorithms (`src/Main.elm`)

| Function | Line | Description |
|---|---|---|
| `updateWeights` | 341 | Hebbian learning: `T = T + S_i * S_j` (outer product, zero diagonal) |
| `updateAsync` | 360 | Asynchronous update: recompute one unit via `sign(dot(row, state))` |
| `updateSync` | 381 | Synchronous update: recompute all units simultaneously |
| `stampMemory` | 397 | Overlay a stored memory's On-pixels onto current state |
| `computeEnergy` | 403 | Hopfield energy: `E = -0.5 * Σ T_ij V_i V_j` |

### Representation

- **UI layer**: `On` / `Off` enum
- **Computation layer**: bipolar `+1` / `-1` integers (converted via `toBipolar` / `toUnitState`)

### Constants

- Grid: N = `rows × cols` units; N varies with `gridSize` (100, 256, or 784)
- Cell size: `280 // cols` px (scales with grid size)
- Memory thumbnail size: `84 // cols` px
- Tick interval: 17ms (~60 FPS) when playing; fires `UpdateRandomUnit` → `UpdateAt i` (random `i`)
- `FlipNBits` flips `max 1 (N * 30 // 784)` bits — proportional to grid size

## Key Conventions

- **Pure functional style** — no ports, no JS interop, no effects beyond random number generation
- **Single module** — all code in `Main.elm`, exposed only `main`
- **Inline CSS** — styling via `Html.Attributes.style`, no external CSS files
- **List-based matrices** — weight matrix and state are plain `List` types (no `Array`)
- **Helper functions** — `chunk`, `getAt`, `replaceAt`, `dotProduct` defined locally
- **Haskell-style doc comments** — some functions use `-- |` prefix

## Dependencies

| Package | Version | Purpose |
|---|---|---|
| elm/browser | 1.0.2 | `Browser.element` app mount |
| elm/core | 1.0.5 | Standard library |
| elm/html | 1.0.0 | HTML rendering |
| elm/random | 1.0.0 | Random unit selection for async updates |
| elm/time | 1.0.0 | Time subscriptions for auto-play |
