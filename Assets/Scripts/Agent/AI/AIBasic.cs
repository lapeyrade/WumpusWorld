using UnityEngine;

namespace Agent.AI
{
    public class AIBasic : MonoBehaviour
    {
        public virtual void FirstTurn()
        {
            GetComponent<Agent>().MoveCell();
            GetComponent<Agent>().SenseCell();
            GetComponent<Agent>().ActionCell();
        }
        
        public virtual void PlayTurn()
        {
            // Generate Objective
            // Generate Action
            GetComponent<Agent>().MoveCell();
            GetComponent<Agent>().SenseCell();
            GetComponent<Agent>().ActionCell();
        }

    }
}
