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

## Test Environment
The tests were conducted using the following hardware and software configuration:
- **Hardware**: 16-inch MacBook Pro M3
  - CPU: 12 cores (6 performance, 6 efficiency)
  - RAM: 36GB
- **Software**: Unity v6000.0.34f1 (Silicon version)

*Note: Performance results may vary depending on the system configuration used.*

Tests using the PrologMetaInterpreter approach on the 100x100 grid configuration could not be completed due to systematic Unity crashes during execution as the Prolog code of this implementation is not optimized and the number of elements become too large.

### Results Storage
The results for each grid configuration are stored in their respective folders:
- [10x10](./10x10)
- [15x15](./15x15)
- [20x20](./20x20)
- [25x25](./25x25)
- [30x30](./30x30)
- [50x50](./50x50)
- [100x100](./100x100)