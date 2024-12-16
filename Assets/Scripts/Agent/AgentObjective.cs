using Ontology;
using UnityEngine;
using GameManagement;

namespace Agent
{
    public class AgentObjective : MonoBehaviour
    {
        // Core components
        private Agent _agent;
        private GameManager _gameManager;

        private void Awake()
        {
            _agent = GetComponent<Agent>();
            _gameManager = GameManager.Instance;
        }

        // Generates objectives based on the agent's personality and environmental conditions
        public void GenerateObjective()
        {
            // Cupid personality seeks wealth when valuable items are present
            if (_agent.GetPersonality<Cupid>() && ExistElementCell<ValuableItem>())
                _agent.SetObjective<Wealth>(true);

            // Brave personality seeks combat when monsters are nearby
            if (_agent.GetPersonality<Brave>() && ExistElementNearCells<Monster>())
                _agent.SetObjective<Fight>(true);

            // Coward personality seeks safety when danger is nearby
            if (_agent.GetPersonality<Coward>() && ExistElementNearCells<IDangerous>())
                _agent.SetObjective<Safety>(true);

            // Ascetic personality seeks abstinence when items are present
            if (_agent.GetPersonality<Ascetic>() && ExistElementCell<Item>())
                _agent.SetObjective<Abstinence>(true);

            // Any personality seeks to be unconstrained when obstacles are present
            if (_agent.GetPersonality<Personality>() && ExistElementCell<Obstacle>())
                _agent.SetObjective<Unconstrained>(true);

            // Any personality seeks to explore when in safe or known areas
            if (_agent.GetPersonality<Personality>() &&
                (ExistTypeCell<SafeCell>() || ExistTypeCell<VisitedCell>() || ExistTypeCell<StartCell>()))
                _agent.SetObjective<Explore>(true);
        }

        // Checks if a specific element exists in the agent's current cell
        // T: The type of element to check for
        public bool ExistElementCell<T>() where T : Element =>
            _gameManager.AgentsMap[_agent.coords.x, _agent.coords.y].Exists(e =>
            {
                var elementType = typeof(T);
                var tagType = System.Type.GetType($"Ontology.{e.tag}");
                return tagType != null && elementType.IsAssignableFrom(tagType);
            });

        // Checks if the agent's current cell is of a specific type
        // T: The type of cell to check for
        public bool ExistTypeCell<T>() where T : Cell =>
            _gameManager.AgentsMap[_agent.coords.x, _agent.coords.y].Exists(e =>
            {
                var elementType = typeof(T);
                var tagType = System.Type.GetType($"Ontology.{e.tag}");
                return tagType != null && elementType.IsAssignableFrom(tagType);
            });

        // Checks if a dangerous element exists in adjacent cells
        // T: The type of dangerous element to check for
        public bool ExistElementNearCells<T>() where T : IDangerous
        {
            // Check each adjacent cell (excluding diagonals)
            for (var x = -1; x <= 1; x++)
            {
                for (var y = -1; y <= 1; y++)
                {
                    // Skip diagonal cells and current cell
                    if (Mathf.Abs(x) == Mathf.Abs(y)) continue;

                    var newX = _agent.coords.x + x;
                    var newY = _agent.coords.y + y;

                    // Check if cell is within grid bounds and contains the dangerous element
                    if (newX >= _gameManager.gridMin.x && newX < _gameManager.gridMax.x
                        && newY >= _gameManager.gridMin.y && newY < _gameManager.gridMax.y
                        && _gameManager.AgentsMap[newX, newY].Exists(e =>
                        {
                            var elementType = typeof(T);
                            var tagType = System.Type.GetType($"Ontology.{e.tag}");
                            return tagType != null && elementType.IsAssignableFrom(tagType);
                        }))
                    {
                        return true;
                    }
                }
            }
            return false;
        }
    }
}