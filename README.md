# Interactive Grass System
**GPU-driven grass with real-time compute shader mesh generation and a paint-to-place editor tool**

---

## Demo

### Lighting & Wind
![Custom lighting and procedural wind animation](Preview/Grass1.gif)
> Real-time grass geometry with custom lighting model and procedural noise-based wind

---

## Features

- **Compute Shader Mesh Generation** — Grass geometry created entirely on the GPU in real-time, no pre-baked meshes
- **Raycast-Based Painting Tool** — Paint grass directly onto surfaces in Play Mode; instantaneous spawning as mesh instances
- **GPU Instancing** — Handles large grass fields efficiently with Unity's GPU instancing pipeline
- **Custom Lighting Shader** — Purpose-built lighting model for realistic grass appearance with proper normals and ambient response
- **Procedural Wind System** — Noise function-driven wind creates natural wave motion across fields, no textures required
- **Player Interaction** — Grass bends and deforms on player contact using shader-based displacement; resets naturally when the player moves away
- **Full Brush Controls** — Adjust brush size, density, blade thickness, blade height, color, and randomness variance in real-time
- **Unity Version Compatibility** — Built in Unity 2022, tested and confirmed working in Unity 6

---

## How It Works

### Mesh Generation
Grass geometry is generated at runtime via compute shaders. When you paint onto a surface, a raycast determines the hit position and the compute shader spawns a new grass blade as an instanced mesh. No texture baking or pre-authoring required — everything is procedural and immediate.

### Lighting Model
Rather than relying on Unity's standard lighting, a custom shader calculates per-blade lighting. This allows direct control over how light interacts with the grass, including subsurface-style translucency at blade tips and proper directional shading across dense fields.

### Wind
A noise function (layered for natural variation) drives per-blade displacement over time. The noise is sampled in world space so the wind feels like a consistent force moving across the field rather than per-object oscillation.

### Player Interaction
A world-space offset is passed to the grass shader representing the player's position and radius. Blades within range are displaced away from the player using a smooth falloff. The displacement decays naturally over time once the player leaves, simulating spring-back.

### Brush Tool
The editor/Play Mode tool uses Unity's `Physics.Raycast` to detect surfaces. Brush parameters are exposed in the Inspector — painting fires multiple raycasts per stroke for density, each spawning a blade with randomized height and thickness within the configured range.

---

## Getting Started

### Requirements
- Unity **2022.3 LTS** or newer (Unity 6 supported)
- **Universal Render Pipeline (URP)**
- A GPU that supports **Compute Shaders** (DirectX 11+, Metal, Vulkan)

### Installation
1. Clone or download this repository
2. Open the project in Unity 2022.3+ with URP configured
3. Open the demo scene in `Assets/Scenes/`

### Usage
1. Enter **Play Mode**
2. Select the **Grass Painter** object in the Hierarchy
3. Use the brush tool to paint grass onto any collider surface
4. Adjust brush parameters in the Inspector panel

---

## Planned
- Open-source release with full package support
- Runtime API for spawning/clearing grass regions via script
- LOD system for very large fields

---

## Technical Stack

| Component | Technology |
|---|---|
| Mesh Generation | Unity Compute Shaders |
| Wind | Procedural Noise (HLSL) |
| Interaction | Shader-based vertex displacement |
| Editor Tool | Unity Play Mode Raycast Painter |

---

*Built with Unity URP. Tested in Unity 2022 and Unity 6.*
