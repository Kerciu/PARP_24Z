# Text Adventure Game

## Overview

Welcome to the **Text Adventure Game**, a Haskell-powered text-based adventure where you embark on a journey through a rich narrative filled with puzzles, characters, and intriguing locations. Explore the game world, collect items, solve challenges, and make decisions that shape your adventure.

---

## Features

- **Inventory Management**: Keep track of the items you collect and use them to solve puzzles or unlock new paths.
- **Exploration**: Navigate through various interconnected locations, each with its own unique details and challenges.
- **Interactions**: Engage with objects and characters in the game world for meaningful and sometimes surprising outcomes.
- **Dynamic Storyline**: Your decisions impact the game, offering multiple possible endings and a highly replayable experience.

---

## Installation

1. **Install required libraries**
   ```bash
   cabal install haskeline
   ```
2. **Clone the repository**:
   ```bash
   git clone https://github.com/Kerciu/PARP_24Z.git
   ```
3. **Navigate to the game directory**:
   ```bash
   cd PARP_24Z/haskell
   ```

---

## How to Play

1. Open your terminal or command prompt.
2. Go to src directory:
   ```bash
   cd src
   ```
3. Launch the game:
   ```bash
   runhaskell Main.hs
   ```

---

## Gameplay Commands

### Exploration
- `look`: Get a description of your current location and discover items or characters.
- `n`, `e`, `s`, `w`: Move in a specific direction (`north`, `east`, `south`or `west`).
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
- `give Character Object`: Give object to to a character.

### Utilities
- `start`: Start/restart the game.
- `instructions`: Display detailed gameplay instructions.
- `quit`: Exit the game.

---

## Project Structure

The game is modular, and each aspect of gameplay is defined in its respective file:

- `Main.hs`: The main entry point of the game. It initializes the game state and starts the game loop.
- `GameState.hs`: Defines the core game state, including the player's inventory, flags, the current location etc.
- `Objects.hs`: Contains definitions for all in-game objects (items) and their descriptions.
- `Interactable.hs`: Defines the `Interactable` data type, representing items the player can interact with.
- `Inventory.hs`: Manages inventory logic, including adding, removing, and displaying items.
- `Locations.hs`: Defines all locations in the game world, including their descriptions and connections.
- `LocationUtils.hs`: Contains utility functions for working with locations, such as finding location details.
- `Movement.hs`: Implements movement logic, allowing the player to navigate between locations and handle locked areas.
- `Interactions.hs`: Handles interactions with characters and items in the game.
- `CommandParser.hs`: Parses user commands and executes the appropriate actions.
- `CommandHandler.hs`: Handles the execution of parsed commands, including inventory management, movement, and interactions.
- `Endings.hs`: Defines different game endings based on the player's choices and actions.
- `Look.hs`: Provides functionality for displaying the current location's description and any items present.
- `Introduction.hs`: Contains the game's introduction text.
- `Instructions.hs`: Contains the game's instructions and available command list.
- `Utils.hs`: Contains utility functions, such as formatted printing for text output.

---
