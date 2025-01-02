using System;
using System.Linq;
using Prolog;
using GameManagement;

namespace Agent.AI
{
    public class AIProlog : AIBasic
    {
        // Core components for Prolog integration
        private PrologInterface _prologInterface;
        private GameManager _gameManager;

        private void Start()
        {
            _gameManager = GameManager.Instance;
            _prologInterface = _gameManager.GetComponent<PrologInterface>();
        }

        public override void FirstTurn()
        {
            _agentMove.MoveCell();    // Move to initial position
            _agentSense.SenseCell();  // Gather initial environment data

            // Initialize the knowledge base with agent information - Add agent position fact
            _prologInterface.QueryText += 
                $", assertz({_agent.tag.ToLower()}([{_agent.name.ToLower()}, [{_agent.coords.x}, {_agent.coords.y}]]))";

            // Add personality traits to knowledge base
            foreach (var perso in _gameManager.personalities.Where(perso => _agent.GetComponent(Type.GetType("Ontology." + perso))))
                    // Add personality fact
                _prologInterface.QueryText += $", assertz({perso.ToString().ToLower()}([{_agent.name.ToLower()}, [_, _]]))";
        }

        public override void PlayTurn()
        {
            // Query the Prolog knowledge base to determine the next action
            switch (_prologInterface.QueryKb(_agent.name, _agent.coords))
            {
                case "attack" or "shoot" or "shootarrow":
                    _agentAction.TryShootingArrow();  // Attack nearby monster
                    break;
                case "pickup":
                    _agentAction.PickUpGold();        // Collect valuable item
                    break;
                case "discard":
                    _agentAction.Discard();           // Remove item from inventory
                    break;
                case "bumpwall":
                    _agentMove.BumpWall();           // Interact with obstacle
                    break;
                case "moveback":
                    _agentMove.MoveAgent(_agentMove.MoveBack());  // Return to previous position
                    break;
                case "move":
                    _agentMove.MoveCell();           // Move to new position
                    break;
            }

            // Update knowledge of environment after action
            _agentSense.SenseCell();
        }
    }
}