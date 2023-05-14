using System.Linq;
using Ontology;
using UnityEngine;

namespace Agent.AI
{
    public class AIFiniteStateMachine : AIBasic
    {
        private enum State
        {
            GenerateObjective,
            GenerateAction,
            GenerateUtility,
            ExecuteHighestUtilityAction,
            SenseCell
        }

        private State _state = State.GenerateObjective;
        
        public override void FirstTurn()
        {
            GetComponent<AgentMove>().MoveCell();
            GetComponent<AgentSense>().SenseCell();
        }

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
                    GetComponents<Component>().Where(c => c is Objective or Move or Action).ToList().ForEach(Destroy);
                    _state = State.GenerateObjective;
                    break;
            }
        }
    }
}
