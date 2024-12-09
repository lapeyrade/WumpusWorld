using System.Linq;
using Ontology;
using TMPro;
using UnityEngine;
using GameManagement;

namespace Agent
{
    public class AgentAction : MonoBehaviour
    {
        private Agent _agent;
        private GameManager _gameManager;
        private TMP_Dropdown _dropdown;

        private void Awake()
        {
            _agent = GetComponent<Agent>();
            _gameManager = GameManager.Instance;
            _dropdown = GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>();
        }

        private Vector2Int Coords => _agent.coords;

        public void GenerateAction()
        {
            // Generate specific actions based on the combination of attached components
            AddComponentIf<Wealth, Cupid, PickUp>();
            AddComponentIf<Abstinence, Ascetic, Discard>();
            AddComponentIf<Safety, Coward, MoveBack>();
            AddComponentIf<Safety, Brave, Attack>();
            AddComponentIf<Fight, Brave, Attack>();
            AddComponentIf<Explore, Personality, Move>();
            AddComponentIf<Unconstrained, Personality, BumpWall>();
        }

        private void AddComponentIf<T1, T2, TComponent>() where TComponent : Component
        {
            // Add TComponent if both T1 and T2 are attached
            if (GetComponent<T1>() is not null && GetComponent<T2>() is not null)
                gameObject.AddComponent<TComponent>();
        }

        public void GenerateUtility()
        {
            // Set utility values based on the combination of attached components
            SetUtilityIf<Cupid, Interact>(5);
            SetUtilityIf<Ascetic, Interact>(3);
            SetUtilityIf<Coward, MoveBack>(10);
            SetUtilityIf<Brave, Attack>(9);
            SetUtilityIf<Personality, Move>(1);
            SetUtilityIf<Personality, BumpWall>(2);
        }

        private void SetUtilityIf<T, TAction>(int value) where TAction : Action
        {
            // Set utility value of TAction if both T and TAction are attached
            if (GetComponent<T>() is not null && GetComponent<TAction>() is not null)
                gameObject.GetComponent<TAction>().Utility = value;
        }

        public void ExecuteHighestUtility()
        {
            // Execute the action with the highest utility value
            var highestUtility = gameObject.GetComponents<Action>().OrderByDescending(c => c.Utility).First();
            highestUtility.Act();

            _dropdown.captionText.text = $"{_agent.name} chose the action {_agent.lastAction} with a utility of {highestUtility.Utility}.";
        }

        public void PickUpGold()
        {
            // Increase the agent's gold count and update the grid accordingly
            _agent.nbGold++;
            GridManager.RemoveFromGrids(Coords, "Gold");
            GridManager.AttachGoldToAgent(_agent);
            _agent.lastAction = "PickUp";
        }

        // remove the gold from the cavern
        internal void Discard() { GridManager.RemoveFromGrids(Coords, "Gold"); }

        public void TryShootingArrow()
        {
            // Check if the agent has arrows available
            if (_agent.nbArrow < 1) return;

            // Shoot arrows in each direction to eliminate Wumpus if present
            ShootIfWumpusExistsInDirection(new Vector2Int(1, 0)); // Right
            ShootIfWumpusExistsInDirection(new Vector2Int(-1, 0)); // Left
            ShootIfWumpusExistsInDirection(new Vector2Int(0, 1)); // Up
            ShootIfWumpusExistsInDirection(new Vector2Int(0, -1)); // Down
        }

        private void ShootIfWumpusExistsInDirection(Vector2Int direction)
        {
            var coordWumpus = Coords;

            while (coordWumpus.x >= _gameManager.gridMin.x && coordWumpus.x < _gameManager.gridMax.x &&
                   coordWumpus.y >= _gameManager.gridMin.y && coordWumpus.y < _gameManager.gridMax.y)
            {
                // Check if a Wumpus is present in the current position
                if (_gameManager.AgentsMap[coordWumpus.x, coordWumpus.y].Exists(e => e.tag is "Wumpus"))
                {
                    // Shoot the Wumpus and update the grid
                    _agent.nbArrow--;
                    GridManager.RemoveFromGrids(coordWumpus, "Wumpus");
                    GridManager.RemoveFromGrids(coordWumpus, "DangerousCell");
                    GridManager.AddToGrids(coordWumpus, "DeadWumpus");
                    GridManager.AddToGrids(coordWumpus, "SafeCell");
                    _agent.lastAction = "ShootArrow";

                    if (coordWumpus.x > Coords.x)
                        _agent.lastAction = "ShootRight";
                    else if (coordWumpus.x < Coords.x)
                        _agent.lastAction = "ShootLeft";
                    else if (coordWumpus.y > Coords.y)
                        _agent.lastAction = "ShootUp";
                    else if (coordWumpus.y < Coords.y)
                        _agent.lastAction = "ShootDown";
                    break;
                }
                coordWumpus += direction;
            }
        }
    }
}