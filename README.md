# Wumpus World

![GameEngine](https://img.shields.io/badge/Game%20Engine-Unity-239120)
![Language](https://img.shields.io/badge/Language-C%23-00cf2c)
![Language](https://img.shields.io/badge/Language-Prolog-ffcc1)
![Open Source](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)

<br/>

This project is an extended version of the classic logic game Wumpus World featured in the book [Artificial intelligence: A Modern Approach](https://aima.cs.berkeley.edu) (Russel & Norvig), inspired by the game [Hunt the Wumpus](https://en.wikipedia.org/wiki/Hunt_the_Wumpus). The game was coded in Unity (so the scripts are in C#). Several "Artificial Intelligence" techniques are available to control the agent, including a [simple set of ad-hoc if-then-else rules](Assets/Scripts/Agent/AI/AIBasic.cs), a [Finite State Machine](Assets/Scripts/Agent/AI/AIFiniteStateMachine.cs) and a [Behavior Tree](Assets/Scripts/Agent/AI/AIBehaviourTree.cs) implementations, all written in C#.
Another AI approach uses [rules and ontologies written in Prolog](Assets/StreamingAssets/article.pl), so an [interface](Assets/Scripts/Prolog/PrologInterface.cs) exists between the Prolog engine and Unity.
Another approach aims to use a [Large Language Model](Assets/Scripts/Agent/AI/AIGpt.cs), but has not yet been finalized.

## Showcase video
A small demo of this project is available by clicking on the following image:

  [![Demo](https://img.youtube.com/vi/dhP5YQKlUbU/0.jpg)](https://youtu.be/dhP5YQKlUbU)

## Game rules
* An agent is in a cave with pits, gold and monsters (Wumpus). The agent must find the gold and exit the cave without dying.
* Pits and Wumpus are deadly and can be detected by the agent since there are breezes and stenches surrounding them.
* The agent can move one cell at a time and kill the wumpus by throwing an arrow in its direction, but its arrows are limited.

# Getting Started

## Dependencies
* **[Unity](https://unity.com/download)**: tested with version 2020 and beyond (latest LTS prefered)
* **[SWI-Prolog](https://www.swi-prolog.org/Download.html)**: tested with version 9 and beyond.

## Setup
* Clone this repository
* Add the cloned repository to your Unity Hub projects (in Unity Hub: click Open -> Add project from disk)
* Run it with Unity (the first time will take some time, as it has to generate a lot of files)
* Select the right Unity Scene

## Run
Choose the settings you want for your game environment :
* Select the "Grid" GameObject
* In the Unity Inspector, adjust the following values for the Game Manager (Script) Component:
  * **Is Game Over**: freeze the game, automatically true when the agent has won or lost the game
  * **Is Mode Auto**: the agents play automatically
    * **Timer Interval**: slider for the interval (in ms) between each agent action call
  * **Random Seed**: number used to generate pseudo-randomly the elements of the grid world (same seed will produce same world)
  * **Grid Min & Max**: minimum and maximum x and y grid coordinates
  * **Tile Size**: space between the cells (only used for visuals)
  * **Nb Pit, Wumpus**, Gold, Agent: set the number of corresponding Pit, Wumpus, Gold and Agents. Their number must not exceed the number of free cells in the grid.
  * **Ai Type**: agent will either be controlled by the set of ad-hoc if-then-else rules, a Finite State Machine, a Behavior Tree, the Prolog with ontologies approach or the LLM (work in progress).
  * **Personalities**: set the personalities of the agent(s), you can add as many as you want (even if some are contradictory)
* Run the game using the play button

<p align="center">
	<img src="https://i.imgur.com/jcIbTZh.png" width="500">
</p>

## Play
- You can use the directional keys (up, bas, rigth, left) to control the agent.
- To make the agent play one action, use the "space" key or the "enter" key if you don't want the agent to always explore the cave in the same way.
- Tick the "Is Mode Auto" box (see the #Run section) to call for new agent actions automatically.

# Credits
* This project is currently under review, so the authors must remain anonymous for the time being.
