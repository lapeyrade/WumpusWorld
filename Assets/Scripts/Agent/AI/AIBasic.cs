using System.Linq;
using Ontology;
using UnityEngine;

namespace Agent.AI
{
    public class AIBasic : MonoBehaviour
    {
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

        // Perform actions on the first turn
        public virtual void FirstTurn()
        {
            _agentMove.MoveCell(); // Move to a cell
            _agentSense.SenseCell(); // Sense the current cell
        }

        public virtual void PlayTurn()
        {
            _agentObjective.GenerateObjective(); // Generate the objective for the agent
            _agentAction.GenerateAction(); // Generate the action for the agent
            _agentAction.GenerateUtility(); // Generate the utility for the agent's actions
            _agentAction.ExecuteHighestUtility(); // Execute the action with the highest utility
            _agentSense.SenseCell(); // Sense the current cell

            // Clean up the components related to Objective, Move, and Action
            GetComponents<Component>().Where(c => c is Objective or Move or Action).ToList().ForEach(Destroy);
        }
    }
}