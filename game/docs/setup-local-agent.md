# Local tooling setup

How the actual hardware maps to jobs, and the exact commands to stand up a
local coding agent for implementation work alongside cloud-Claude planning
sessions. Nothing here was run by an agent -- these are commands for a human
to run at each physical machine; no sandboxed session has network access to
this hardware.

## Hardware -> role

| Machine | Specs | Role |
|---|---|---|
| Razer Blade 14 (2025) | RTX 3080 Ti laptop GPU (16GB VRAM), 16GB system RAM, Linux, run headless off an external NVMe over USB-C | Local model inference server |
| Framework 13 (2025, AMD Ryzen AI board) | 32GB RAM, no discrete GPU, Linux | Daily-driver dev machine: Godot editor, Rust toolchain, Aider client |
| Steam Deck OLED (1TB) | AMD APU, SteamOS/Linux | Playtest target only -- validates the dual-stick control scheme (Decisions Log entry 11) and real SteamOS/Linux export, not a dev box |

16GB VRAM is enough for a genuinely useful quantized coding model, but not a
32B one without offloading layers to system RAM (slow, and that machine only
has 16GB of it) -- default to a 14B-class model.

**Honest caveat:** a local 14B model, even a good coding-tuned one, will
struggle noticeably more with Rust specifically than a frontier hosted model
would -- the borrow checker and trait system punish exactly the kind of
subtle mistakes smaller models make. Plan the task split below around that,
don't just point it at the whole codebase.

## Blade: model server

```sh
curl -fsSL https://ollama.com/install.sh | sh

# Ollama binds to localhost only by default -- open it to the LAN so the
# Framework can reach it.
sudo systemctl edit ollama
# add under [Service]:
#   Environment="OLLAMA_HOST=0.0.0.0"
sudo systemctl restart ollama

ollama pull qwen2.5-coder:14b

hostname -I   # note this LAN IP, needed on the Framework side
```

Only bind `OLLAMA_HOST` to your LAN interface, not `0.0.0.0` on an untrusted
network -- this exposes the model API to anything that can reach the Blade.

## Framework: dev machine + Aider client

```sh
# Rust toolchain
curl https://sh.rustup.rs -sSf | sh

# Godot 4.6+: AppImage from godotengine.org, or your distro's Flatpak.
# No discrete GPU on this machine -- use the Compatibility renderer, not
# Forward+, when creating/importing the project.

# Aider, pointed at the Blade's Ollama server
pipx install aider-chat
export OLLAMA_API_BASE=http://<blade-lan-ip>:11434
aider --model ollama/qwen2.5-coder:14b
```

## Task split

| Good fit for the local agent (Aider) | Keep with cloud planning session |
|---|---|
| GDScript scene/UI wiring, once a pattern is established | New Rust/gdext systems: `voxel-world`, `bullet-sim`, hull-sim (fire-spread) |
| Data/content tables (weapons, chassis, encounters) | Anything touching the GDExtension boundary shape |
| Additional test coverage for existing logic | Tricky trait/lifetime/ownership design |
| Docs maintenance, repetitive refactors with a clear existing pattern | Cross-cutting architecture changes |

Rule of thumb: build a new Rust system here first as a working reference,
*then* hand Aider similar follow-on work (a second weapon-pattern module
that mirrors an existing one, more tests, etc.) rather than starting it cold
on unfamiliar ground.
