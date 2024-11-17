# Text Adventure Game

## Overview

Welcome to the **Text Adventure Game**, a Prolog-powered text-based adventure where you embark on a journey through a rich narrative filled with puzzles, characters, and intriguing locations. Explore the game world, collect items, solve challenges, and make decisions that shape your adventure.

This game is designed to offer a blend of storytelling and logical problem-solving, leveraging Prolog's unique capabilities for dynamic gameplay.

---

## Features

- **Inventory Management**: Keep track of the items you collect and use them to solve puzzles or unlock new paths.
- **Exploration**: Navigate through various interconnected locations, each with its own unique details and challenges.
- **Interactions**: Engage with objects and characters in the game world for meaningful and sometimes surprising outcomes.
- **Dynamic Storyline**: Your decisions impact the game, offering multiple possible endings and a highly replayable experience.
- **Rich Prolog Logic**: A demonstration of Prolog’s power in handling dynamic rules, states, and interactions.

---

## Requirements

To run this game, you need the following:
- **SWI-Prolog** or any compatible Prolog interpreter.
- Basic knowledge of Prolog commands.

---

## Installation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/yourusername/text-adventure-game.git
   ```
2. **Navigate to the game directory**:
   ```bash
   cd text-adventure-game
   ```
3. **Ensure all game files are present**:
   - `game.pl`
   - `src/objects.pl`
   - `src/movement.pl`
   - `src/locations.pl`
   - `src/interactions.pl`
   - `src/endings.pl`

---

## How to Play

1. Open your terminal or command prompt.
2. Start SWI-Prolog (or another Prolog interpreter):
   ```bash
   swipl
   ```
3. Load the main game file:
   ```prolog
   ?- [game].
   ```
4. Begin your adventure:
   ```prolog
   ?- start.
   ```

---

## Gameplay Commands

### Exploration
- `look.`: Get a description of your current location and discover items or characters.
- `n.`, `e.`, `s.`, `w.`: Move in a specific direction (`north`, `east`, `south`or `west`).

### Inventory Management
- `take(Item).`: Pick up an item and add it to your inventory.
- `drop(Item).`: Remove an item from your inventory.
- `check(Item).`: Check the description of an item or interact with it.
- `inventory.`: Check what items you're carrying.

### Interactions
- `interact(Character).`: Engage in dialogue or perform actions with a character.
- `give(Character, Object).`: Give object to to a character.

### Utilities
- `start.`: Start the game.
- `instructions.`: Display detailed gameplay instructions.
- `halt.`: Exit the game.

---

## Project Structure

The game is modular, and each aspect of gameplay is defined in its respective file:
- `game.pl`: Main game logic and entry point.
- `objects.pl`: Rules and data for game objects and inventory.
- `movement.pl`: Rules defining movement logic to different locations.
- `locations.pl`: Definitions for all locations in the game world.
- `interactions.pl`: Scripts for interacting with characters and objects.
- `endings.pl`: Logic for determining the game's possible outcomes.

---
