using UnityEngine;

namespace Agent
{
    public class AgentAI : MonoBehaviour
    {
        private Agent _agent;
    
        public void Start()
        {
            _agent = gameObject.GetComponent<Agent>();
        }

        public void PlayTurn()
        {
            _agent.MoveCell();
            _agent.SenseCell();
            _agent.ActionCell();
        }
    
    }
}
