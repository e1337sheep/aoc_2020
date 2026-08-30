//! Pure-Rust loadout balancing simulation core.
//!
//! This is a straight port of loadout-balancing logic that started life as a
//! JavaScript prototype used in a playtesting design artifact. The formulas
//! below are intentionally kept byte-for-byte equivalent to that artifact so
//! the two stay in sync -- do not "improve" the math here without updating
//! the JS source of truth first.
//!
//! The crate has zero dependency on Godot; it is meant to be consumed by
//! `godot-bridge` (or any other frontend, or plain unit tests) as a small,
//! deterministic pure function: [`compute_loadout`].

use std::fmt;

/// A hull chassis: the base platform a loadout is built on.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Chassis {
    pub id: &'static str,
    pub name: &'static str,
    pub mass_cap: f64,
    pub energy_cap: f64,
    pub base_speed: f64,
    pub base_turn: f64,
    pub weapon_slots: u32,
}

/// A weapon that can be equipped into one of a chassis's weapon slots.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Weapon {
    pub id: &'static str,
    pub name: &'static str,
    pub mass: f64,
    pub energy: f64,
    pub dps: f64,
}

/// A utility module. Up to two may be equipped per loadout, independent of
/// weapon slots. Utilities can raise a chassis's mass or energy caps.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Utility {
    pub id: &'static str,
    pub name: &'static str,
    pub mass: f64,
    pub energy_cap_bonus: f64,
    pub mass_cap_bonus: f64,
}

/// Maximum number of utility modules a loadout may equip, regardless of
/// chassis.
pub const MAX_UTILITY_SLOTS: usize = 2;

pub const CHASSIS: &[Chassis] = &[
    Chassis {
        id: "interceptor",
        name: "Interceptor",
        mass_cap: 60.0,
        energy_cap: 40.0,
        base_speed: 1.3,
        base_turn: 1.3,
        weapon_slots: 2,
    },
    Chassis {
        id: "multirole",
        name: "Multirole",
        mass_cap: 100.0,
        energy_cap: 70.0,
        base_speed: 1.0,
        base_turn: 1.0,
        weapon_slots: 3,
    },
    Chassis {
        id: "gunship",
        name: "Gunship",
        mass_cap: 160.0,
        energy_cap: 100.0,
        base_speed: 0.75,
        base_turn: 0.7,
        weapon_slots: 4,
    },
];

pub const WEAPONS: &[Weapon] = &[
    Weapon { id: "pulse", name: "Pulse Cannon", mass: 8.0, energy: 6.0, dps: 18.0 },
    Weapon { id: "spread", name: "Spread Gun", mass: 14.0, energy: 12.0, dps: 26.0 },
    Weapon { id: "homing", name: "Homing Launcher", mass: 20.0, energy: 16.0, dps: 22.0 },
    Weapon { id: "beam", name: "Beam Laser", mass: 26.0, energy: 24.0, dps: 40.0 },
    Weapon { id: "mines", name: "Mine Layer", mass: 18.0, energy: 8.0, dps: 14.0 },
    Weapon { id: "flak", name: "Flak Cannon", mass: 22.0, energy: 18.0, dps: 30.0 },
];

pub const UTILITY: &[Utility] = &[
    Utility { id: "reactor", name: "Reactor", mass: 12.0, energy_cap_bonus: 20.0, mass_cap_bonus: 0.0 },
    Utility { id: "frame", name: "Reinforced Frame", mass: 0.0, energy_cap_bonus: 0.0, mass_cap_bonus: 25.0 },
];

/// Look up a chassis by id.
pub fn find_chassis(id: &str) -> Option<&'static Chassis> {
    CHASSIS.iter().find(|c| c.id == id)
}

/// Look up a weapon by id.
pub fn find_weapon(id: &str) -> Option<&'static Weapon> {
    WEAPONS.iter().find(|w| w.id == id)
}

/// Look up a utility module by id.
pub fn find_utility(id: &str) -> Option<&'static Utility> {
    UTILITY.iter().find(|u| u.id == id)
}

/// A chosen loadout: a chassis plus the weapon and utility ids equipped on
/// it. Ids are validated (and slot counts enforced) by [`compute_loadout`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoadoutSelection {
    pub chassis_id: String,
    pub weapon_ids: Vec<String>,
    pub utility_ids: Vec<String>,
}

impl LoadoutSelection {
    pub fn new(
        chassis_id: impl Into<String>,
        weapon_ids: impl IntoIterator<Item = impl Into<String>>,
        utility_ids: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        Self {
            chassis_id: chassis_id.into(),
            weapon_ids: weapon_ids.into_iter().map(Into::into).collect(),
            utility_ids: utility_ids.into_iter().map(Into::into).collect(),
        }
    }
}

/// Everything that can go wrong resolving a [`LoadoutSelection`] into
/// [`LoadoutStats`].
#[derive(Debug, Clone, PartialEq)]
pub enum LoadoutError {
    UnknownChassis(String),
    UnknownWeapon(String),
    UnknownUtility(String),
    TooManyWeapons { chassis_id: String, slots: u32, equipped: usize },
    TooManyUtilities { max: usize, equipped: usize },
}

impl fmt::Display for LoadoutError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoadoutError::UnknownChassis(id) => write!(f, "unknown chassis id: {id}"),
            LoadoutError::UnknownWeapon(id) => write!(f, "unknown weapon id: {id}"),
            LoadoutError::UnknownUtility(id) => write!(f, "unknown utility id: {id}"),
            LoadoutError::TooManyWeapons { chassis_id, slots, equipped } => write!(
                f,
                "chassis '{chassis_id}' has {slots} weapon slot(s) but {equipped} weapon(s) were equipped"
            ),
            LoadoutError::TooManyUtilities { max, equipped } => write!(
                f,
                "at most {max} utility module(s) may be equipped, got {equipped}"
            ),
        }
    }
}

impl std::error::Error for LoadoutError {}

/// The computed, derived stats for a resolved loadout.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LoadoutStats {
    pub used_mass: f64,
    pub mass_cap: f64,
    pub used_energy: f64,
    pub energy_cap: f64,
    pub mass_ratio: f64,
    pub energy_ratio: f64,
    pub speed_mult: f64,
    pub turn_mult: f64,
    pub duty_cycle: f64,
    pub nominal_dps: f64,
    pub effective_dps: f64,
}

/// Resolve a [`LoadoutSelection`] against the static [`CHASSIS`], [`WEAPONS`]
/// and [`UTILITY`] tables and compute its derived [`LoadoutStats`].
///
/// Formulas (kept in lockstep with the JS design-artifact source of truth):
///
/// ```text
/// speed_mult = if mass_ratio <= 1.0 { 1.0 + 0.3*(1.0-mass_ratio) }
///              else { max(0.3, 1.0 - 0.8*(mass_ratio-1.0)) }
/// turn_mult  = if mass_ratio <= 1.0 { 1.0 + 0.4*(1.0-mass_ratio) }
///              else { max(0.2, 1.0 - 1.2*(mass_ratio-1.0)) }
/// duty_cycle = if energy_ratio <= 1.0 { 1.0 } else { energy_cap / used_energy }
/// ```
pub fn compute_loadout(selection: &LoadoutSelection) -> Result<LoadoutStats, LoadoutError> {
    let chassis = find_chassis(&selection.chassis_id)
        .ok_or_else(|| LoadoutError::UnknownChassis(selection.chassis_id.clone()))?;

    if selection.weapon_ids.len() > chassis.weapon_slots as usize {
        return Err(LoadoutError::TooManyWeapons {
            chassis_id: chassis.id.to_string(),
            slots: chassis.weapon_slots,
            equipped: selection.weapon_ids.len(),
        });
    }

    if selection.utility_ids.len() > MAX_UTILITY_SLOTS {
        return Err(LoadoutError::TooManyUtilities {
            max: MAX_UTILITY_SLOTS,
            equipped: selection.utility_ids.len(),
        });
    }

    let weapons: Vec<&'static Weapon> = selection
        .weapon_ids
        .iter()
        .map(|id| find_weapon(id).ok_or_else(|| LoadoutError::UnknownWeapon(id.clone())))
        .collect::<Result<_, _>>()?;

    let utilities: Vec<&'static Utility> = selection
        .utility_ids
        .iter()
        .map(|id| find_utility(id).ok_or_else(|| LoadoutError::UnknownUtility(id.clone())))
        .collect::<Result<_, _>>()?;

    let weapon_mass: f64 = weapons.iter().map(|w| w.mass).sum();
    let weapon_energy: f64 = weapons.iter().map(|w| w.energy).sum();
    let nominal_dps: f64 = weapons.iter().map(|w| w.dps).sum();

    let utility_mass: f64 = utilities.iter().map(|u| u.mass).sum();
    let mass_cap_bonus: f64 = utilities.iter().map(|u| u.mass_cap_bonus).sum();
    let energy_cap_bonus: f64 = utilities.iter().map(|u| u.energy_cap_bonus).sum();

    let used_mass = weapon_mass + utility_mass;
    let mass_cap = chassis.mass_cap + mass_cap_bonus;
    let used_energy = weapon_energy;
    let energy_cap = chassis.energy_cap + energy_cap_bonus;

    let mass_ratio = used_mass / mass_cap;
    let energy_ratio = used_energy / energy_cap;

    let speed_mult = if mass_ratio <= 1.0 {
        1.0 + 0.3 * (1.0 - mass_ratio)
    } else {
        (1.0 - 0.8 * (mass_ratio - 1.0)).max(0.3)
    };

    let turn_mult = if mass_ratio <= 1.0 {
        1.0 + 0.4 * (1.0 - mass_ratio)
    } else {
        (1.0 - 1.2 * (mass_ratio - 1.0)).max(0.2)
    };

    let duty_cycle = if energy_ratio <= 1.0 {
        1.0
    } else {
        energy_cap / used_energy
    };

    let effective_dps = nominal_dps * duty_cycle;

    Ok(LoadoutStats {
        used_mass,
        mass_cap,
        used_energy,
        energy_cap,
        mass_ratio,
        energy_ratio,
        speed_mult,
        turn_mult,
        duty_cycle,
        nominal_dps,
        effective_dps,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    const EPS: f64 = 1e-6;

    fn approx_eq(a: f64, b: f64) -> bool {
        (a - b).abs() < EPS
    }

    #[test]
    fn multirole_pulse_spread_known_good() {
        let selection = LoadoutSelection::new("multirole", ["pulse", "spread"], Vec::<String>::new());
        let stats = compute_loadout(&selection).expect("valid loadout");

        assert!(approx_eq(stats.used_mass, 22.0), "used_mass = {}", stats.used_mass);
        assert!(approx_eq(stats.mass_cap, 100.0), "mass_cap = {}", stats.mass_cap);
        assert!(approx_eq(stats.used_energy, 18.0), "used_energy = {}", stats.used_energy);
        assert!(approx_eq(stats.energy_cap, 70.0), "energy_cap = {}", stats.energy_cap);
        assert!(approx_eq(stats.speed_mult, 1.234), "speed_mult = {}", stats.speed_mult);
        assert!(approx_eq(stats.turn_mult, 1.312), "turn_mult = {}", stats.turn_mult);
        assert!(approx_eq(stats.duty_cycle, 1.0), "duty_cycle = {}", stats.duty_cycle);
        assert!(approx_eq(stats.nominal_dps, 44.0), "nominal_dps = {}", stats.nominal_dps);
        assert!(approx_eq(stats.effective_dps, 44.0), "effective_dps = {}", stats.effective_dps);
    }

    #[test]
    fn too_many_weapons_is_rejected() {
        // Interceptor has only 2 weapon slots.
        let selection = LoadoutSelection::new(
            "interceptor",
            ["pulse", "spread", "homing"],
            Vec::<String>::new(),
        );
        let err = compute_loadout(&selection).expect_err("should reject over-slotted loadout");
        assert_eq!(
            err,
            LoadoutError::TooManyWeapons {
                chassis_id: "interceptor".to_string(),
                slots: 2,
                equipped: 3,
            }
        );
    }

    #[test]
    fn too_many_utilities_is_rejected() {
        let selection = LoadoutSelection::new(
            "multirole",
            ["pulse"],
            ["reactor", "frame", "reactor"],
        );
        let err = compute_loadout(&selection).expect_err("should reject over-slotted utilities");
        assert_eq!(err, LoadoutError::TooManyUtilities { max: 2, equipped: 3 });
    }

    #[test]
    fn unknown_chassis_is_rejected() {
        let selection = LoadoutSelection::new("dreadnought", Vec::<String>::new(), Vec::<String>::new());
        assert_eq!(
            compute_loadout(&selection),
            Err(LoadoutError::UnknownChassis("dreadnought".to_string()))
        );
    }

    #[test]
    fn unknown_weapon_is_rejected() {
        let selection = LoadoutSelection::new("interceptor", ["railgun"], Vec::<String>::new());
        assert_eq!(
            compute_loadout(&selection),
            Err(LoadoutError::UnknownWeapon("railgun".to_string()))
        );
    }

    #[test]
    fn overloaded_mass_ratio_exercises_penalty_branch() {
        // Interceptor: mass_cap = 60, energy_cap = 40, 2 weapon slots.
        // Duplicate weapon ids are permitted (a loadout is just a list of
        // slot picks), so equip both slots with 'beam' (mass 26 each = 52)
        // plus the 'reactor' utility (mass 12): used_mass = 64 > mass_cap
        // 60, genuinely overloaded.
        let selection = LoadoutSelection::new("interceptor", ["beam", "beam"], ["reactor"]);
        let stats = compute_loadout(&selection).expect("valid loadout");

        assert!(approx_eq(stats.used_mass, 64.0), "used_mass = {}", stats.used_mass);
        assert!(approx_eq(stats.mass_cap, 60.0), "mass_cap = {}", stats.mass_cap);
        assert!(stats.mass_ratio > 1.0, "mass_ratio = {}", stats.mass_ratio);

        let expected_speed = (1.0 - 0.8 * (stats.mass_ratio - 1.0)).max(0.3);
        let expected_turn = (1.0 - 1.2 * (stats.mass_ratio - 1.0)).max(0.2);
        assert!(approx_eq(stats.speed_mult, expected_speed), "speed_mult = {}", stats.speed_mult);
        assert!(approx_eq(stats.turn_mult, expected_turn), "turn_mult = {}", stats.turn_mult);

        // used_energy = 24 + 24 (beam x2) = 48; energy_cap = 40 + 20 (reactor) = 60.
        // energy_ratio = 0.8 <= 1.0, so duty_cycle stays 1.0 -- this case
        // isolates the mass-overload branch without also triggering the
        // energy one.
        assert!(approx_eq(stats.used_energy, 48.0));
        assert!(approx_eq(stats.energy_cap, 60.0));
        assert!(approx_eq(stats.duty_cycle, 1.0));
    }

    #[test]
    fn overloaded_energy_ratio_reduces_duty_cycle() {
        // Interceptor: energy_cap = 40, 2 weapon slots.
        // Two 'beam' weapons (energy 24 each = 48) with no utility:
        // energy_ratio = 48 / 40 = 1.2 > 1.0, so duty_cycle = energy_cap / used_energy.
        let selection = LoadoutSelection::new("interceptor", ["beam", "beam"], Vec::<String>::new());
        let stats = compute_loadout(&selection).expect("valid loadout");

        assert!(approx_eq(stats.used_energy, 48.0));
        assert!(approx_eq(stats.energy_cap, 40.0));
        assert!(stats.energy_ratio > 1.0);
        assert!(approx_eq(stats.duty_cycle, 40.0 / 48.0), "duty_cycle = {}", stats.duty_cycle);
        assert!(approx_eq(stats.nominal_dps, 80.0));
        assert!(approx_eq(stats.effective_dps, 80.0 * (40.0 / 48.0)));
    }
}
