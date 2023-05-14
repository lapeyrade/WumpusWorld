using System.Linq;
using Ontology;
using TMPro;
using UnityEngine;

namespace Agent
{
    public class AgentAction : MonoBehaviour
    {
        private Agent Agent => GetComponent<Agent>();
        private Vector2Int Coords => Agent.coords;

        public void GenerateAction()
        {
            // Generate specific actions based on the combination of attached components
            AddComponentIf<Wealth, Cupid, PickUp>();
            AddComponentIf<Abstinence, Ascetic, Drop>();
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

            // Update the UI dropdown text to display the chosen action and its utility
            GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>().captionText.text = $"{Agent.name} chose the" +
                $" action {highestUtility.GetType().Name} with a utility of {highestUtility.Utility}.";
        }

        public void PickUpGold()
        {
            // Increase the agent's gold count and update the grid accordingly
            GetComponent<Agent>().nbGold++;
            GridManager.RemoveFromGrids(Coords, "Gold");
            GridManager.AttachGoldToAgent(GetComponent<Agent>());
        }

        public void TryShootingArrow()
        {
            // Check if the agent has arrows available
            if (GetComponent<Agent>().nbArrow < 1) return;

            // Shoot arrows in each direction to eliminate Wumpus if present
            ShootIfWumpusExistsInDirection(new Vector2Int(1, 0)); // Right
            ShootIfWumpusExistsInDirection(new Vector2Int(-1, 0)); // Left
            ShootIfWumpusExistsInDirection(new Vector2Int(0, 1)); // Up
            ShootIfWumpusExistsInDirection(new Vector2Int(0, -1)); // Down
        }

        private void ShootIfWumpusExistsInDirection(Vector2Int direction)
        {
            var currCoords = Coords;

            while (IsInBounds(currCoords))
            {
                // Check if a Wumpus is present in the current position
                if (GameManager.Instance.AgentsMap[currCoords.x, currCoords.y].Exists(e => e.tag is "Wumpus"))
                {
                    // Shoot the Wumpus and update the grid
                    ShootWumpus(currCoords);
                    break;
                }
                currCoords += direction;
            }
        }

        private static bool IsInBounds(Vector2Int coords) =>
            coords.x >= GameManager.Instance.gridMin.x && coords.x < GameManager.Instance.gridMax.x &&
            coords.y >= GameManager.Instance.gridMin.y && coords.y < GameManager.Instance.gridMax.y;

        private void ShootWumpus(Vector2Int coordWumpus)
        {
            // Decrease the agent's arrow count and update the grid to reflect the shot Wumpus
            Agent.nbArrow--;
            GridManager.RemoveFromGrids(coordWumpus, "Wumpus");
            GridManager.RemoveFromGrids(coordWumpus, "DangerousCell");
            GridManager.AddToGrids(coordWumpus, "DeadWumpus");
            GridManager.AddToGrids(coordWumpus, "SafeCell");
        }
    }
}