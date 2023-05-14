using Ontology;
using UnityEngine;

namespace Agent
{
    public class AgentObjective : MonoBehaviour
    {
        private Agent _agent;
        
        private void Start() => _agent = GetComponent<Agent>();
        
        // Generates an objective based on the agent's personality and surroundings
        public void GenerateObjective()
        {
            if (ExistPersonality<Cupid>() && ExistElementCell<ValuableItem>())
                gameObject.AddComponent<Wealth>();
            if (ExistPersonality<Brave>() && ExistElementNearCells<Monster>())
                gameObject.AddComponent<Fight>();
            if (ExistPersonality<Coward>() && ExistElementNearCells<IDangerous>())
                gameObject.AddComponent<Safety>();
            if (ExistPersonality<Ascetic>() && ExistElementCell<Item>())
                gameObject.AddComponent<Abstinence>();
            if (ExistPersonality<Personality>() && ExistElementCell<Obstacle>())
                gameObject.AddComponent<Unconstrained>();
            if (ExistPersonality<Personality>() &&
                (ExistTypeCell<SafeCell>() || ExistTypeCell<VisitedCell>() || ExistTypeCell<StartCell>()))
                gameObject.AddComponent<Explore>();
        }
        
        // Checks if the agent has a specific personality
        public bool ExistPersonality<T>() where T : Personality => GetComponent<T>() is not null;
        
        // Checks if the agent has a specific element in its cell
        public bool ExistElementCell<T>() where T : Element =>
            GameManager.Instance.AgentsMap[_agent.coords.x, _agent.coords.y].Exists(e => e.GetComponent<T>());

        // Checks if there is a specific Cell type in the agent's cell
        public bool ExistTypeCell<T>() where T : Cell =>
            GameManager.Instance.AgentsMap[_agent.coords.x, _agent.coords.y].Exists(e => e.GetComponent<T>());

        // Checks if there is a specific element in the agent's surroundings
        public bool ExistElementNearCells<T>() where T : IDangerous
        {
            for (var x = -1; x <= 1; x++)
            {
                for (var y = -1; y <= 1; y++)
                {
                    if (Mathf.Abs(x) == Mathf.Abs(y)) continue;
                    
                    var newX = _agent.coords.x + x;
                    var newY = _agent.coords.y + y;

                    if (GameManager.IsWithinGrid(newX, newY)
                        && GameManager.Instance.AgentsMap[newX, newY].Exists(e => e.GetComponent(typeof(T))))
                        return true;
                }
            }
            return false;
        }
    }
}