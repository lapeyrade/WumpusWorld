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
        
        // The first turn of the agent is different from the rest
        public override void FirstTurn()
        {
            GetComponent<AgentMove>().MoveCell();
            GetComponent<AgentSense>().SenseCell();
        }

        // The finite state machine
        public override void PlayTurn()
        {
            switch (_state)
            {
                case State.GenerateObjective:
                    GetComponent<AgentObjective>().GenerateObjective();
                    _state = State.GenerateAction;
                    break;
                case State.GenerateAction:
                    GetComponent<AgentAction>().GenerateAction();
                    _state = State.GenerateUtility;
                    break;
                case State.GenerateUtility:
                    GetComponent<AgentAction>().GenerateUtility();
                    _state = State.ExecuteHighestUtilityAction;
                    break;
                case State.ExecuteHighestUtilityAction:
                    GetComponent<AgentAction>().ExecuteHighestUtility();
                    _state = State.SenseCell;
                    break;
                case State.SenseCell:
                default :
                    GetComponent<AgentSense>().SenseCell();
                    // Clean up the components related to Objective, Move, and Action
                    GetComponents<Component>().Where(c => c is Objective or Move or Action)
                        .ToList().ForEach(Destroy);
                    _state = State.GenerateObjective;
                    break;
            }
        }
    }
}
