using UnityEngine;

namespace Agent.AI
{
    public class AIBasic : MonoBehaviour
    {
        // Core agent components
        public Agent _agent;
        protected AgentMove _agentMove;
        protected AgentSense _agentSense;
        protected AgentObjective _agentObjective;
        protected AgentAction _agentAction;

        // Initialize agent components
        protected void Awake()
        {
            _agent = GetComponent<Agent>();
            _agentMove = GetComponent<AgentMove>();
            _agentSense = GetComponent<AgentSense>();
            _agentObjective = GetComponent<AgentObjective>();
            _agentAction = GetComponent<AgentAction>();
        }

        // Initialize basic agent behavior on first turn
        public virtual void FirstTurn()
        {
            _agentMove.MoveCell();    // Move to initial cell position
            _agentSense.SenseCell();  // Gather initial environment information
        }

        // Execute the agent's decision-making cycle each turn
        public virtual void PlayTurn()
        {
            // 1. Decision Making Phase
            _agentObjective.GenerateObjective();  // Determine what the agent wants to achieve
            _agentAction.GenerateAction();        // Generate possible actions
            _agentAction.GenerateUtility();       // Evaluate action utilities

            // 2. Execution Phase
            _agentAction.ExecuteHighestUtility(); // Perform the most beneficial action
            _agentSense.SenseCell();             // Update environment knowledge

            // 3. Cleanup Phase
            _agent.ResetObjectives();  // Clear objectives for next turn
            _agent.ResetActions();     // Reset actions and utilities for next turn
        }
    }
}