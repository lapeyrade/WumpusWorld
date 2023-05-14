using System.Linq;
using Ontology;
using UnityEngine;

namespace Agent.AI
{
    public class AIBasic : MonoBehaviour
    {
        // Perform actions on the first turn
        public virtual void FirstTurn()
        {
            GetComponent<AgentMove>().MoveCell(); // Move to a cell
            GetComponent<AgentSense>().SenseCell(); // Sense the current cell
        }
        public virtual void PlayTurn()
        {
            GetComponent<AgentObjective>().GenerateObjective(); // Generate the objective for the agent
            GetComponent<AgentAction>().GenerateAction(); // Generate the action for the agent
            GetComponent<AgentAction>().GenerateUtility(); // Generate the utility for the agent's actions
            GetComponent<AgentAction>().ExecuteHighestUtility(); // Execute the action with the highest utility
            GetComponent<AgentSense>().SenseCell(); // Sense the current cell
            
            // Clean up the components related to Objective, Move, and Action
            GetComponents<Component>().Where(c => c is Objective or Move or Action).ToList().ForEach(Destroy);
        }
    }
}