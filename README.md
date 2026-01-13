 The Obsidian Chest

> A text-based dungeon crawler written in Haskell.

## 📖 Overview

**The Obsidian Chest** is an interactive fiction game developed as a university coursework project. It places the player in a dark dungeon where they must navigate traps, solve puzzles, and defeat monsters to unlock the mysteries of the central Hall.

The project demonstrates core functional programming concepts, utilizing **recursive game loops**, **pattern matching** for command parsing, and **immutable state transitions** to track game progress.

**The Goal:** You are trapped. To escape, you must find three keys to unlock the massive Obsidian Chest in the center of the hall.

## 🚀 Features

* **Custom Natural Language Parser:** interpret commands like `look`, `take sword`, or `ignite 2,4`.
* **Combat Mechanics:** A strategic combat system requiring the player to `attack`, `block`, or `parry` based on enemy cues.
* **Progression System:** The game world evolves (rooms change state) as the player solves puzzles and defeats bosses.
* **Puzzle Solving:** Includes logic puzzles (torch combinations) and environmental storytelling.

## 🛠 Prerequisites

To run this game, you need the **GHC (Glasgow Haskell Compiler)** installed.

* **Haskell GHC** (Base installation is sufficient)
* **Libraries:** Uses standard `Data.Map` and `Data.Maybe` (included in base).

## 🎮 How to Run

1. **Save the code:**
Save the source code into a file named `game.hs`.
2. **Compile:**
Open your terminal/command prompt and run:
```bash
ghc --make game.hs -o dungeon

```


3. **Run:**
```bash
./dungeon

```


*(On Windows, run `dungeon.exe`)*

## 🕹️ How to Play

The game accepts two-word commands (Verb + Noun) or single-word commands.

### Basic Commands

* **Navigation:** `north`, `south`, `east`, `west`
* **Interaction:** `look`, `search`, `open`, `take [item]`
* **Meta:** `look` (to see the room description again)

### Combat & Puzzles

* **Fighting:**
* `attack [target]`: Strike the enemy.
* `block [target]`: Defend against an incoming hit.
* `parry [target]`: Deflect an attack to leave the enemy vulnerable.


* **Specific Actions:**
* `ignite [combination]`: Used for the torch puzzle (e.g., `ignite 1,3`).
* `usekey`: Unlock specific doors or objects.



### Example Gameplay

```text
> You are in a dimly lit hallway. To the north is a heavy door.

> look door
A solid concrete door, with a glowing red orb above.

> north
The door is locked.

> search soldier
You search the soldiers body and find a key in his pouch. Key Acquired!

```

## 📂 Code Structure

The project is contained in a single module but is structured as follows:

* **`PredefinedAction` & `Action`:** Data types defining all possible verbs (`Look`, `Parry`, `Ignite`, etc.).
* **`Room` & `Object`:** The world is built using a `Map` of Room IDs. Each room contains a local Map of objects (like "chest", "monster").
* **`getPlayerAction`:** A custom IO parser that reads user input, tokenizes it, and matches it to `Action` types.
* **`game` (The Loop):** The main recursive function. It takes the current `GameMap` and `RoomId`. When the player moves or changes the world state, the function calls itself with the *new* RoomId, effectively simulating state without using Mutable Variables.

## 🎓 Learning Outcomes

This project focuses on:

* **Functional IO:** separating pure logic (result evaluation) from input/output.
* **Algebraic Data Types:** Modeling game results (`PlayerDeath`, `GameWon`, `Move`).
* **Recursion:** Managing the game loop and state transitions.
* **Map Operations:** Efficiently looking up rooms and objects using `Data.Map`.

## 👤 Author

* **Yusuf Minhas** - *University of West London*

