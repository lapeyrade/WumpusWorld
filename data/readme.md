# Configuration Parameters

## Environment Parameters
- **randomSeed**: Seed used to generate the environment (for reproducibility)
- **gridMax**: Maximum coordinates of the grid
- **nbPit**: Number of pits in the environment
- **nbWumpus**: Number of wumpus in the environment
- **nbGold**: Number of gold in the environment
- **nbAgent**: Number of agents in the environment
- **aiType**: Type of AI used by the agent
- **personalities**: Personalities of the agent

## Agent Personalities
The following personalities are implemented:
- **brave**: Agent attempts to kill monsters (wumpus) when adjacent
- **cupid**: Agent picks up gold when on the same cell
- **ascetic**: Agent throws away gold when on the same cell
- **coward**: Agent retreats to previous cell when adjacent to danger (wumpus, pit)

## AI Implementation Types
The following AI approaches are implemented:
- **Basic**: IF-THEN-ELSE approach
- **FiniteStateMachine**: Finite State Machine approach
- **BehaviorTree**: Behavior Tree approach
- **Prolog**: Prolog engine approach
- **PrologMetaInterpreter**: Prolog meta-interpreter approach

## Test Configurations
The number of pits and wumpus is calculated using: `gridMax * gridMax * 0.03`.
    
    e.g. for a 10x10 grid, there are 10*10*0.03=3 pits and wumpus.


Common parameters across all configurations:
| Parameter | Value |
|:----------|:------|
| **randomSeed** | 1 |
| **nbGold** | 1 |
| **nbAgent** | [1, 5, 10] |
| **aiType** | [Basic, FiniteStateMachine, BehaviorTree, Prolog, PrologMetaInterpreter] |
| **personalities** | [(cupid & brave), (ascetic & coward), (cupid & coward)] |

### Grid-Specific Configurations

| Grid Size | Number of Pits & Wumpus |
|:---------:|:-------------:|
| 10x10     | 3          |
| 15x15     | 7          |
| 20x20     | 12         |
| 25x25     | 19         |
| 30x30     | 27         |
| 50x50     | 75         |
| 100x100   | 300        |

#### Number of configurations:
We have 7 grid sizes, 3 personality sets, 5 AI types, and 3 agent numbers.

So, there are `7*3*5*3=315` configurations.
