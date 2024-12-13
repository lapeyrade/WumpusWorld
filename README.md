# WumpusWorld

![GameEngine](https://img.shields.io/badge/Game%20Engine-Unity-239120)
![Language](https://img.shields.io/badge/Language-C%23-00cf2c)
![Language](https://img.shields.io/badge/Language-Prolog-ffcc1)
![Open Source](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)
  
<br/>

This project is an extended version of the classic logic game Wumpus World featured in the book [Artificial intelligence: A Modern Approach by Russel & Norvig](https://aima.cs.berkeley.edu), inspired by the game [Hunt the Wumpus](https://en.wikipedia.org/wiki/Hunt_the_Wumpus). The game was coded in Unity using C# scripts. Several "Artificial Intelligence" techniques are available to control the agent, including a [simple set of ad-hoc if-then-else rules](Assets/Scripts/Agent/AI/AIBasic.cs), a [Finite State Machine](Assets/Scripts/Agent/AI/AIFiniteStateMachine.cs) and a [Behavior Tree](Assets/Scripts/Agent/AI/AIBehaviourTree.cs) implementations, all written in C#.
Another AI approach based on [logic programming rules and ontologies in Prolog](Assets/StreamingAssets/article.pl), so an [interface](Assets/Scripts/Prolog/PrologInterface.cs) exists between the Prolog engine and Unity.
Another approach aims to use a [Large Language Model](Assets/Scripts/Agent/AI/AILargeLanguageModel.cs) (LLM), but has not yet been finalized.


## Showcase video
A small demo of this project is available by clicking on the following image:

  [![Demo](https://img.youtube.com/vi/dhP5YQKlUbU/0.jpg)](https://youtu.be/dhP5YQKlUbU)

In the video, the screen is split into two parts:

1. Left side: The world as seen by the agent
2. Right side: The actual world state

The cells in the world are color-coded as follows:

- **Gray**: Start location
- **Light Blue**: Visited cells
- **Green**: Safe cells (surrounded by at least one cell without breeze or stench)
- **Red**: Dangerous cells (containing a Wumpus or pit)
- **Orange**: Uncertain cells
- **Black**: Walls (inaccessible to the agent)

## Rules of the game
* The game takes place in a cave-like grid world containing pits, gold, and monsters called Wumpus.
* The objective is for an agent to find the gold and safely exit the cave without dying.
* Pits and Wumpus are deadly to the agent if entered.
* The agent can detect nearby dangers:
  * Breeze indicates an adjacent pit
  * Stench indicates an adjacent Wumpus
* The agent can:
  * Move one cell at a time in any cardinal direction
  * Shoot arrows to kill a Wumpus (limited number of arrows)
* The game ends when the agent:
  * Dies by falling into a pit or encountering a Wumpus
  * Successfully retrieves the gold and exits the cave

# Getting Started

## Dependencies
* **[Unity](https://unity.com/download)**: tested with version 6 and beyond, see the [ProjectVersion.txt](ProjectSettings/ProjectVersion.txt) file for exact version used.
* **[SWI-Prolog](https://www.swi-prolog.org/Download.html)**: tested with version 9 and beyond.

## Setup
1. Clone this repository to your local machine using e.g.:
   ```
   git clone https://github.com/sylvainlapeyrade/WumpusWorld.git
   cd wumpus-world-ai
   ```
2. Install SWI-Prolog:
   - Visit the [SWI-Prolog download page](https://www.swi-prolog.org/Download.html)
   - Download and install the version appropriate for your operating system
   - Ensure that SWI-Prolog is added to your system's PATH
2. Open Unity Hub and add the cloned project:
   - Click on "Open" in Unity Hub
   - Select "Add project from disk"
   - Navigate to and select the cloned repository folder
3. Open the project in Unity:
   - The first time you open it, Unity will generate necessary files (this may take a few minutes)
4. Once the project is loaded, select the correct Unity Scene:
   - In the Project window, navigate to the Scenes folder
   - Double-click on the main game scene

## Run
<p>
	<img src="https://i.imgur.com/jcIbTZh.png" width="500">
</p>

Configure your game environment by following these steps:

1. In the Unity Hierarchy, select the "Grid" GameObject.
2. In the Unity Inspector, locate the Game Manager Script Component and adjust these settings:

   * **Is Game Over**: Freezes the game. Automatically set to true when the agent wins or loses.
   * **Is Mode Auto**: Enables automatic play for agents.
     * **Timer Interval**: Set the delay (in milliseconds) between each agent action.
   * **Random Seed**: Enter a number to generate a specific grid layout. The same seed always produces the same world.
   * **Grid Min & Max**: Define the grid's dimensions by setting minimum and maximum x and y coordinates.
   * **Tile Size**: Adjust the visual spacing between cells (affects appearance only).
   * **Nb Pit, Wumpus, Gold, Agent**: Specify the quantity of each element. Ensure the total doesn't exceed available grid cells.
   * **AI Type**: Choose the agent's control method:
     - Ad-hoc if-then-else rules
     - Finite State Machine
     - Behavior Tree
     - Prolog with ontologies
     - LLM (Large Language Model) - currently in development
   * **Personalities**: Assign personality traits to your agent(s). You can add multiple traits, even if they conflict.

3. Click the play button in Unity to start the game with your chosen settings.

## Play
- Manual Control:
  - Use the directional keys (up, down, right, left) to move the agent.
- Semi-Automatic Play:
  - Press the "space" key to make the agent perform one action.
  - Use the "return" key for a randomized action, adding variety to the agent's exploration.
- Fully Automatic Play:
  - Enable the "Is Mode Auto" option in the Game Manager settings (see the [Run](#Run) section) for continuous automatic agent actions.

## Repository structure
```
WumpusWorld
├── Assets
│   ├── Scripts
│   │   ├── Agent
│   │   │   ├── AI
│   │   │   │   ├── AIBasic.cs
│   │   │   │   ├── AIBehaviourTree.cs
│   │   │   │   ├── AIFiniteStateMachine.cs
│   │   │   │   ├── AILargeLanguageModel.cs
│   │   │   │   ├── AIProlog.cs
│   │   │   ├── Agent.cs
│   │   │   ├── AgentAction.cs
│   │   │   ├── AgentMove.cs
│   │   │   ├── AgentObjective.cs
│   │   │   ├── AgentSense.cs
│   │   ├── Ontology
│   │   │   ├── Action.cs
│   │   │   ├── Cell.cs
│   │   │   ├── Element.cs
│   │   │   ├── Objective.cs
│   │   │   ├── Personality.cs
│   │   ├── Prolog
│   │   ├── CameraController.cs
│   │   ├── GameController.cs
│   │   ├── GameManager.cs
│   │   ├── GridBuilder.cs
```