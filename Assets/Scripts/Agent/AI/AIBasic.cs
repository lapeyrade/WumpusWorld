using System.Linq;
using Ontology;
using UnityEngine;

namespace Agent.AI
{
    public class AIBasic : MonoBehaviour
    {
        public virtual void FirstTurn()
        {
            GetComponent<AgentMove>().MoveCell();
            GetComponent<AgentSense>().SenseCell();
        }
        
        public virtual void PlayTurn()
        {
            GetComponent<AgentObjective>().GenerateObjective();
            GetComponent<AgentAction>().GenerateAction();
            GetComponent<AgentAction>().GenerateUtility();
            GetComponent<AgentAction>().ExecuteHighestUtility();
            GetComponent<AgentSense>().SenseCell();
            GetComponents<Component>().Where(c => c is Objective or Move or Action).ToList().ForEach(Destroy);
        }
    }
}
