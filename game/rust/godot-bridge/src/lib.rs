//! GDExtension entry point bridging the Godot editor/runtime to `sim-core`.
//!
//! This crate is intentionally thin: it owns no simulation logic itself. It
//! exposes a single Godot-callable node, `SimBridge`, whose job is to accept
//! plain Godot values (strings and string arrays), hand them to
//! `sim_core::compute_loadout`, and marshal the result back into a Godot
//! `Dictionary`. This is the "hello cube" round-trip proof for the
//! Rust <-> Godot boundary: a real, small, useful call, not a placeholder.

use godot::prelude::*;

use sim_core::{compute_loadout, LoadoutSelection};

struct BulletHellExtension;

#[gdextension]
unsafe impl ExtensionLibrary for BulletHellExtension {}

/// A minimal Godot node that exposes `sim-core`'s loadout balancing
/// calculation to GDScript.
///
/// Attach this as an autoload or a plain child node, then call
/// [`SimBridge::compute_loadout`] from GDScript with a chassis id, a list of
/// weapon ids, and a list of utility ids to get back a `Dictionary` of the
/// derived stats.
#[derive(GodotClass)]
#[class(base=Node)]
struct SimBridge {
    base: Base<Node>,
}

#[godot_api]
impl INode for SimBridge {
    fn init(base: Base<Node>) -> Self {
        Self { base }
    }
}

#[godot_api]
impl SimBridge {
    /// Compute derived loadout stats for a chassis + equipped weapons +
    /// equipped utilities.
    ///
    /// On success, returns a `Dictionary` with keys: `used_mass`,
    /// `mass_cap`, `used_energy`, `energy_cap`, `mass_ratio`,
    /// `energy_ratio`, `speed_mult`, `turn_mult`, `duty_cycle`,
    /// `nominal_dps`, `effective_dps`.
    ///
    /// On failure (unknown id, or too many weapons/utilities equipped for
    /// the chassis), returns a `Dictionary` with a single `error` key
    /// describing what went wrong, so GDScript callers can check
    /// `result.has("error")` rather than dealing with a Rust `Result`
    /// across the FFI boundary.
    #[func]
    fn compute_loadout(
        &self,
        chassis_id: GString,
        weapon_ids: PackedStringArray,
        utility_ids: PackedStringArray,
    ) -> VarDictionary {
        let selection = LoadoutSelection::new(
            chassis_id.to_string(),
            weapon_ids.as_slice().iter().map(|s| s.to_string()),
            utility_ids.as_slice().iter().map(|s| s.to_string()),
        );

        match compute_loadout(&selection) {
            Ok(stats) => vdict! {
                "used_mass" => stats.used_mass,
                "mass_cap" => stats.mass_cap,
                "used_energy" => stats.used_energy,
                "energy_cap" => stats.energy_cap,
                "mass_ratio" => stats.mass_ratio,
                "energy_ratio" => stats.energy_ratio,
                "speed_mult" => stats.speed_mult,
                "turn_mult" => stats.turn_mult,
                "duty_cycle" => stats.duty_cycle,
                "nominal_dps" => stats.nominal_dps,
                "effective_dps" => stats.effective_dps,
            },
            Err(err) => vdict! {
                "error" => err.to_string(),
            },
        }
    }
}
