# Smalltalk Text Adventure Game

## Authors
Miłosz Andryszczuk  
Kacper Górski  
Szymon Kamiński  

## Overview

Welcome to the **Text Adventure Game**, a Smalltalk-powered text-based adventure where you embark on a journey through a rich narrative filled with puzzles, characters, and intriguing locations. Explore the game world, collect items, solve challenges, and make decisions that shape your adventure.

---

## Features

- **Inventory Management**: Keep track of the items you collect and use them to solve puzzles or unlock new paths.
- **Exploration**: Navigate through various interconnected locations, each with its own unique details and challenges.
- **Interactions**: Engage with objects and characters in the game world for meaningful and sometimes surprising outcomes.
- **Dynamic Storyline**: Your decisions impact the game, offering multiple possible endings and a highly replayable experience.

---

## Prerequisites

1. **Install GNU Smalltalk**:
   ```bash
   sudo apt install gnu-smalltalk
   ```

2. **Verify installation**:
   ```bash
   gst --version
   ```

---

## Installation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/Kerciu/PARP_24Z.git
   ```

---

## How to Play

1. Open your terminal or command prompt.
2. Navigate to the directory containing the `run` file:
   ```bash
   cd PARP_24Z/smalltalk
   ```
3. Launch the game:
   ```bash
   ./run
   ```

---

## Gameplay Commands

### Exploration
- `look`: Get a description of your current location and discover items or characters.
- `n`, `e`, `s`, `w`: Move in a specific direction (`north`, `east`, `south`, or `west`).
- `enter_car`: Enter the car.
- `exit_car`: Exit the car.

### Inventory Management
- `take Item`: Pick up an item and add it to your inventory.
- `drop Item`: Remove an item from your inventory.
- `check Item`: Check the description of an item or interact with it.
- `open Item`: Open the item if it's possible.
- `inventory`: Check what items you're carrying.

### Interactions
- `interact Character`: Engage in dialogue or perform actions with a character.
- `give Character Object`: Give an object to a character.

### Utilities
- `start`: Start/restart the game.
- `instructions`: Display detailed gameplay instructions.
- `quit`: Exit the game.

---

## Project Structure

The game is modular, and each aspect of gameplay is defined in its respective file:

- `adventure.st`: Defines the main game logic, including the game loop and command parsing.
- `endings.st`: Contains definitions for different possible endings based on player choices and actions.
- `gamestate.st`: Defines the core game state, including the player's inventory, flags, and the current location.
- `items.st`: Contains definitions for all in-game objects (items) and their descriptions.
- `locations.st`: Defines all locations in the game world, including their descriptions and connections.
- `look.txt`: Provides text files for location descriptions that depend on other conditions.
- `run`: A script to start the game using GNU Smalltalk.

---
