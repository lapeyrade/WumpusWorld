using System;
using System.Linq;
using Prolog;
using GameManagement;

namespace Agent.AI
{
    public class AIProlog : AIBasic
    {
        private AgentMove _agentMove;
        private AgentSense _agentSense;
        private AgentAction _agentAction;
        private Agent _agent;
        private PrologInterface _prologInterface;
        private GameManager _gameManager;

        private void Start()
        {
            _agentMove = GetComponent<AgentMove>();
            _agentSense = GetComponent<AgentSense>();
            _agentAction = GetComponent<AgentAction>();
            _agent = GetComponent<Agent>();
            _gameManager = GameManager.Instance;
            _prologInterface = _gameManager.GetComponent<PrologInterface>();
        }

        public override void FirstTurn()
        {
            _agentMove.MoveCell(); // Move to a cell
            _agentSense.SenseCell(); // Sense the current cell

            // Update the knowledge base
            _prologInterface.QueryText +=
                $", assertz(data_concept([{_agent.name.ToLower()}, [{_agent.coords.x}, {_agent.coords.y}]], {_agent.tag.ToLower()})), " +
                $"assertz({_agent.tag.ToLower()}([{_agent.name.ToLower()}, [{_agent.coords.x}, {_agent.coords.y}]]))";

            foreach (var perso in _gameManager.personalities.Where(perso => _agent.GetComponent(Type.GetType("Ontology." + perso))))
                _prologInterface.QueryText +=
                    $", assertz(has_personality_trait([{_agent.name.ToLower()}, [_, _]], {perso.ToString().ToLower()})), " +
                    $"assertz({perso.ToString().ToLower()}([{_agent.name.ToLower()}, [{_agent.coords.x}, {_agent.coords.y}]]))";
        }

        public override void PlayTurn()
        {
            // Query the knowledge base for agent actions
            switch (_prologInterface.QueryKb(_agent.name, _agent.coords))
            {
                case "attack" or "shoot" or "shootarrow": // Try shooting an arrow
                    _agentAction.TryShootingArrow();
                    break;
                case "pickup": // Pick up gold
                    _agentAction.PickUpGold();
                    break;
                case "discard": // Discard the gold (destroy the GameObject)
                    _agentAction.Discard();
                    break;
                case "bumpwall": // Bump into a wall
                    _agentMove.BumpWall();
                    break;
                case "moveback": // Move agent back to a previous cell
                    _agentMove.MoveAgent(_agentMove.MoveBack());
                    break;
                case "move": // Move to a cell
                    _agentMove.MoveCell();
                    break;
            }

            // Sense the element in the current cell
            _agentSense.SenseCell();
        }
    }
}