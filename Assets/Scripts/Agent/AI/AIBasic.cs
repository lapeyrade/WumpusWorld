using System.Linq;
using Ontology;
using TMPro;
using UnityEngine;
using Action = Ontology.Action;

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
            
            var highestUtilityComponent = GetComponent<Agent>().GetComponents<Component>()
                .Where(c => c is Action)
                .OrderByDescending(c => c.GetComponent<Action>().Utility)
                .First().GetComponent<Action>();
            
            highestUtilityComponent.Act();
            
            GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>().captionText.text =
                $"{GetComponent<Agent>().name} chose the action {highestUtilityComponent} with a utility of" +
                $" {highestUtilityComponent.GetComponent<Action>().Utility}.";

            GetComponent<AgentSense>().SenseCell();
            
            GetComponents<Component>().Where(c => c is Objective or Move or Action).ToList().ForEach(Destroy);
        }

    }
}
