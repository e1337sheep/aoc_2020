# Vector Corridor Prototype (working title)

Bullet-hell spaceship shooter prototype. This is **milestone M0 + M1** of the
project roadmap: proving the Rust <-> Godot round-trip and porting the
loadout-balancing simulation core. It is not a playable slice yet -- there is
no scene, no ship, no bullets on screen.

## Layout

```
game/
  godot/                  Godot 4.6 project (editor project, no gameplay yet)
    project.godot
    bullet_hell.gdextension
  rust/                   Cargo workspace
    sim-core/              Pure-Rust loadout balancing logic, zero Godot deps
    godot-bridge/           GDExtension entry point, depends on sim-core + godot crate
```

`sim-core` is a straight port of the loadout-balancing math from a JS design
artifact the user has been playtesting -- chassis / weapons / utilities,
mass and energy caps, speed/turn multipliers, duty cycle and effective DPS.
Keep the formulas in `game/rust/sim-core/src/lib.rs` in lockstep with that
JS source of truth; don't tweak the numbers here without updating there too.

`godot-bridge` is intentionally thin: it's the GDExtension `cdylib` that
Godot loads, exposing a `SimBridge` node with a `compute_loadout` method
GDScript can call directly. It owns no simulation logic of its own -- it
just marshals Godot values in, calls `sim_core::compute_loadout`, and
marshals the result back out as a `Dictionary`.

## Building / testing the Rust side

From `game/rust/`:

```sh
cargo test -p sim-core       # sim-core unit tests (pure Rust, no Godot needed)
cargo build -p godot-bridge  # builds the GDExtension cdylib
```

Both commands were run and verified passing as part of this bootstrap.

## Opening the Godot project

Open `game/godot/project.godot` in a local Godot **4.6+** editor. This step
was **not** verified by the agent that bootstrapped this scaffold -- there is
no Godot editor available in that sandbox, so "it opens" is unconfirmed.
Please open it yourself and report back if anything is off (in particular,
double-check `bullet_hell.gdextension`'s library paths once you have real
`debug`/`release` builds in `game/rust/target/`).

## Status

- **M0 (Rust <-> Godot round-trip):** done. `SimBridge.compute_loadout()` is
  a real, working call across the FFI boundary, not a placeholder.
- **M1 (sim-core port):** done, with unit tests covering the known-good
  balance case, slot-limit rejection, and both the mass-overload and
  energy-overload penalty branches.
- Everything after this (actual scenes, ship movement, bullets, enemy
  patterns) is future work.
