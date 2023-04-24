using System.Linq;
using Ontology;
using UnityEngine;

namespace Agent.AI
{
    public class AIBasic : MonoBehaviour
    {
        public virtual void FirstTurn()
        {
            GetComponent<Agent>().MoveCell();
            GetComponent<Agent>().SenseCell();
        }
        
        public virtual void PlayTurn()
        {
            GetComponent<Agent>().SenseCell();
            
            GetComponent<Agent>().GenerateObjective();
            GetComponent<Agent>().GenerateAction();

            if (GetComponent<Agent>().GetComponent<Attack>())
                GetComponent<AgentAction>().TryShootingArrow();
            else if (GetComponent<Agent>().GetComponent<PickUp>())
                GetComponent<AgentAction>().PickUpGold();
            else if (GetComponent<Agent>().GetComponent<Drop>())
                return;
            else if (GetComponent<Agent>().GetComponent<BumpWall>())
                GetComponent<AgentMove>().BumpWall();
            else if (GetComponent<Agent>().GetComponent<MoveBack>())
                GetComponent<AgentMove>().MoveAgent(GetComponent<AgentMove>().MoveBack());
            else if (GetComponent<Agent>().GetComponent<AgentMove>())
                GetComponent<Agent>().MoveCell();
            
            GetComponent<Agent>().SenseCell();
            
            foreach (var component in GetComponents<Component>().Where(c => c is Objective or Move or Action))
            {
                Debug.Log(component);
                Destroy(component);
            }
        }

    }
}
