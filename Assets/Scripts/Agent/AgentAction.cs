using Ontology;
using TMPro;
using UnityEngine;
using GameManagement;

namespace Agent
{
    public class AgentAction : MonoBehaviour
    {
        // Core components
        private Agent _agent;
        private GameManager _gameManager;
        private TMP_Dropdown _dropdown;

        private void Awake()
        {
            _agent = GetComponent<Agent>();
            _gameManager = GameManager.Instance;
            _dropdown = GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>();
        }

        // Shorthand property for agent coordinates
        private Vector2Int Coords => _agent.coords;

        public void GenerateAction()
        {
            // Generate specific actions based on personality and objective combinations
            if (_agent.GetObjective<Wealth>() && _agent.GetPersonality<Cupid>())
                _agent.SetAction<PickUp>(true);      // Enable pickup for wealth-seeking Cupids
            if (_agent.GetObjective<Abstinence>() && _agent.GetPersonality<Ascetic>())
                _agent.SetAction<Discard>(true);     // Enable discard for abstinent Ascetics
            if (_agent.GetObjective<Safety>() && _agent.GetPersonality<Coward>())
                _agent.SetAction<MoveBack>(true);    // Enable retreat for safety-seeking Cowards
            if (_agent.GetObjective<Safety>() && _agent.GetPersonality<Brave>())
                _agent.SetAction<Attack>(true);      // Enable attack for safety-seeking Brave agents
            if (_agent.GetObjective<Fight>() && _agent.GetPersonality<Brave>())
                _agent.SetAction<Attack>(true);      // Enable attack for fight-seeking Brave agents
            if (_agent.GetObjective<Explore>() && _agent.GetPersonality<Personality>())
                _agent.SetAction<Move>(true);        // Enable movement for exploring agents
            if (_agent.GetObjective<Unconstrained>() && _agent.GetPersonality<Personality>())
                _agent.SetAction<BumpWall>(true);    // Enable wall interaction for unconstrained agents
        }

        public void GenerateUtility()
        {
            // Assign utility values based on personality and action combinations
            if (_agent.GetPersonality<Coward>() && _agent.GetAction<MoveBack>())
                _agent.GetAction<MoveBack>().Utility = 10;    // Cowards highly value retreating
            if (_agent.GetPersonality<Brave>() && _agent.GetAction<Attack>())
                _agent.GetAction<Attack>().Utility = 9;       // Brave agents value attacking highly
            if (_agent.GetPersonality<Cupid>() && _agent.GetAction<Interact>())
                _agent.GetAction<Interact>().Utility = 5;     // Cupids value interaction highly
            if (_agent.GetPersonality<Ascetic>() && _agent.GetAction<Interact>())
                _agent.GetAction<Interact>().Utility = 3;     // Ascetics value interaction moderately
            if (_agent.GetPersonality<Personality>() && _agent.GetAction<BumpWall>())
                _agent.GetAction<BumpWall>().Utility = 2;     // Wall interaction has low utility
            if (_agent.GetPersonality<Personality>() && _agent.GetAction<Move>())
                _agent.GetAction<Move>().Utility = 1;         // Basic movement has low utility
        }

        public void ExecuteHighestUtility()
        {
            // Execute the action with the highest utility value
            var highestUtility = _agent.GetHighestUtilityAction();
            highestUtility.Act();

            // Update UI to show the chosen action and its utility
            _dropdown.captionText.text =
            $"{_agent.name} chose the action {_agent.lastAction} with a utility of {highestUtility.Utility}.";
        }

        public void PickUpGold()
        {
            // Handle gold collection logic
            _agent.nbGold++;                         // Increment agent's gold count
            GridManager.RemoveFromGrids(Coords, "Gold");  // Remove gold from the grid
            GridManager.AttachGoldToAgent(_agent);        // Visually attach gold to agent
            _agent.lastAction = "PickUp";                 // Record the action
        }

        // Remove gold from the current cell
        internal void Discard() { GridManager.RemoveFromGrids(Coords, "Gold"); }

        // Shoot an arrow in a given direction (used by the LLM AI)
        public void ShootArrow(Vector2Int direction)
        {
            if (_agent.nbArrow < 1) return;
            _agent.nbArrow--;

            var targetCoord = Coords + direction;

            if (GridManager.CellInGridLimits(targetCoord) &&
                _gameManager.Map[targetCoord.x, targetCoord.y].Exists(e => e.CompareTag("Wumpus")))
            {
                GridManager.RemoveFromGrids(targetCoord, "Wumpus");
                GridManager.RemoveFromGrids(targetCoord, "DangerousCell");
                GridManager.AddToGrids(targetCoord, "DeadWumpus");
                GridManager.AddToGrids(targetCoord, "SafeCell");
            }

            if (direction == Vector2Int.right) _agent.lastAction = "ShootRight";
            else if (direction == Vector2Int.left) _agent.lastAction = "ShootLeft";
            else if (direction == Vector2Int.up) _agent.lastAction = "ShootUp";
            else if (direction == Vector2Int.down) _agent.lastAction = "ShootDown";
        }

        public void TryShootingArrow()
        {
            // Verify arrow availability
            if (_agent.nbArrow < 1) return;

            // Try shooting in adjacent cells in all four directions
            ShootIfWumpusExistsInDirection(new Vector2Int(1, 0));  // Right
            ShootIfWumpusExistsInDirection(new Vector2Int(-1, 0)); // Left
            ShootIfWumpusExistsInDirection(new Vector2Int(0, 1));  // Up
            ShootIfWumpusExistsInDirection(new Vector2Int(0, -1)); // Down
        }

        private void ShootIfWumpusExistsInDirection(Vector2Int direction)
        {
            var coordWumpus = Coords + direction;

            // Check if adjacent cell is within grid boundaries
            if (coordWumpus.x >= _gameManager.gridMin.x && coordWumpus.x < _gameManager.gridMax.x &&
                coordWumpus.y >= _gameManager.gridMin.y && coordWumpus.y < _gameManager.gridMax.y)
            {
                // If Wumpus found in adjacent cell, handle the shooting logic
                if (_gameManager.AgentsMap[coordWumpus.x, coordWumpus.y].Exists(e => e.tag is "Wumpus"))
                {
                    _agent.nbArrow--;                                     // Use an arrow
                    GridManager.RemoveFromGrids(coordWumpus, "Wumpus");         // Remove Wumpus
                    GridManager.RemoveFromGrids(coordWumpus, "DangerousCell"); // Update cell danger status
                    GridManager.AddToGrids(coordWumpus, "DeadWumpus");         // Add dead Wumpus marker
                    GridManager.AddToGrids(coordWumpus, "SafeCell");           // Mark cell as safe

                    // Record shooting direction
                    _agent.lastAction = coordWumpus.x > Coords.x ? "ShootRight" :
                                      coordWumpus.x < Coords.x ? "ShootLeft" :
                                      coordWumpus.y > Coords.y ? "ShootUp" : "ShootDown";
                }
            }
        }
    }
}