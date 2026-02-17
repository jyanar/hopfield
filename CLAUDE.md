# CLAUDE.md

## Project Overview

Hopfield is an interactive browser-based Hopfield neural network simulator written in [Elm](https://elm-lang.org/) (v0.19.1). It provides a 28x28 grid where users can draw patterns, store them as memories using Hebbian learning, and observe the network recall stored patterns through asynchronous or synchronous updates. The live app is deployed at https://yanar.org/hopfield.

## Repository Structure

```
hopfield/
├── src/
│   └── Main.elm          # Entire application (single module)
├── .github/
│   └── workflows/
│       └── elm-to-gh-pages.yml  # CI/CD: build & deploy to GitHub Pages
├── elm.json              # Elm project config and dependencies
├── index.html            # HTML shell that mounts the Elm app
├── .gitignore            # Ignores elm-stuff/
└── README.md
```

This is a single-file Elm application. All logic — model, update, view, and helper functions — lives in `src/Main.elm`.

## Build & Run

### Prerequisites

- [Elm](https://guide.elm-lang.org/install/elm.html) 0.19.1

### Build

```sh
elm make src/Main.elm --output=main.js --optimize
```

This compiles `src/Main.elm` into `main.js`, which is loaded by `index.html`.

### Run locally

After building, open `index.html` in a browser. No dev server is required.

### Elm Reactor (development)

```sh
elm reactor
```

Then visit `http://localhost:8000/src/Main.elm` in a browser.

## Testing

No test framework is configured. The `test-dependencies` in `elm.json` are empty. If tests are added, use [elm-test](https://github.com/elm-explorations/test).

## Linting & Formatting

No linting or formatting tools are configured. The standard tool for Elm formatting is [elm-format](https://github.com/avh4/elm-format).

## CI/CD

GitHub Actions workflow (`.github/workflows/elm-to-gh-pages.yml`):
- **Trigger**: Push to `main` branch (or manual `workflow_dispatch`)
- **Steps**: Checkout → Install Elm 0.19.1 → Build → Deploy to GitHub Pages
- **Concurrency**: One deployment at a time; queued runs wait (no cancellation of in-progress)

## Architecture

The app follows **The Elm Architecture (TEA)**:

### Model (`src/Main.elm:41-47`)

```elm
type alias Model =
    { state   : List UnitState        -- 784 neurons (28×28), each On or Off
    , weights : List (List Int)       -- 784×784 weight matrix
    , mems    : List (List UnitState) -- stored memory patterns
    , playing : Bool                  -- simulation auto-running
    , drawing : Bool                  -- user dragging to draw
    }
```

### Key Types

- `UnitState` — `On | Off` enum for neuron activation
- `Msg` — union type with all user interactions and system events (defined at line 62)

### Core Algorithms (`src/Main.elm`)

| Function | Line | Description |
|---|---|---|
| `updateWeights` | 273 | Hebbian learning: `T = T + S_i * S_j` (outer product, zero diagonal) |
| `updateAsync` | 292 | Asynchronous update: recompute one unit via `sign(dot(row, state))` |
| `updateSync` | 312 | Synchronous update: recompute all units simultaneously |
| `stampMemory` | 329 | Overlay a stored memory's On-pixels onto current state |

### Representation

- **UI layer**: `On` / `Off` enum
- **Computation layer**: bipolar `+1` / `-1` integers (converted via `toBipolar` / `toUnitState`)

### Constants

- Grid: 28 rows × 28 columns = 784 units
- Tick interval: 17ms (~60 FPS) when playing
- Cell size: 10px (grid), 3px (memory thumbnails)

## Key Conventions

- **Pure functional style** — no ports, no JS interop, no effects beyond random number generation
- **Single module** — all code in `Main.elm`, exposed only `main`
- **Inline CSS** — styling via `Html.Attributes.style`, no external CSS files
- **List-based matrices** — weight matrix and state are plain `List` types (no Array)
- **Helper functions** — utility functions like `chunk`, `getAt`, `replaceAt`, `dotProduct` are defined locally
- **Haskell-style doc comments** — some functions use `-- |` prefix for documentation

## Dependencies

| Package | Version | Purpose |
|---|---|---|
| elm/browser | 1.0.2 | `Browser.element` app mount |
| elm/core | 1.0.5 | Standard library |
| elm/html | 1.0.0 | HTML rendering |
| elm/random | 1.0.0 | Random unit selection for async updates |
| elm/time | 1.0.0 | Time subscriptions for auto-play |
