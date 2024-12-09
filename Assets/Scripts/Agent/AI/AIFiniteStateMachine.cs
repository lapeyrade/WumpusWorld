using System.Linq;
using Ontology;
using UnityEngine;

namespace Agent.AI
{
    public class AIFiniteStateMachine : AIBasic
    {
        // Define the possible states for the finite state machine
        private enum State
        {
            GenerateObjective,
            GenerateAction,
            GenerateUtility,
            ExecuteHighestUtilityAction,
            SenseCell
        }

        private State _state = State.GenerateObjective;
        // Cached components
        private AgentMove _agentMove;
        private AgentSense _agentSense;
        private AgentObjective _agentObjective;
        private AgentAction _agentAction;

        private void Awake()
        {
            _agentMove = GetComponent<AgentMove>();
            _agentSense = GetComponent<AgentSense>();
            _agentObjective = GetComponent<AgentObjective>();
            _agentAction = GetComponent<AgentAction>();
        }

        // The first turn of the agent is different from the rest
        public override void FirstTurn()
        {
            _agentMove.MoveCell();
            _agentSense.SenseCell();
        }

        // The finite state machine
        public override void PlayTurn()
        {
            switch (_state)
            {
                case State.GenerateObjective:
                    _agentObjective.GenerateObjective();
                    _state = State.GenerateAction;
                    break;
                case State.GenerateAction:
                    _agentAction.GenerateAction();
                    _state = State.GenerateUtility;
                    break;
                case State.GenerateUtility:
                    _agentAction.GenerateUtility();
                    _state = State.ExecuteHighestUtilityAction;
                    break;
                case State.ExecuteHighestUtilityAction:
                    _agentAction.ExecuteHighestUtility();
                    _state = State.SenseCell;
                    break;
                case State.SenseCell:
                default:
                    _agentSense.SenseCell();
                    // Clean up the components related to Objective, Move, and Action
                    GetComponents<Component>().Where(c => c is Objective or Move or Action)
                        .ToList().ForEach(Destroy);
                    _state = State.GenerateObjective;
                    break;
            }
        }
    }
}
