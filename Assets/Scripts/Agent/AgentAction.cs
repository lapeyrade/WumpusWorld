using System.Linq;
using Ontology;
using TMPro;
using UnityEngine;
using GameManagement;

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

            // Update the UI dropdown text to display the chosen action and its utility
            GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>().captionText.text = $"{Agent.name} chose the" +
                $" action {Agent.lastAction} with a utility of {highestUtility.Utility}.";
        }

        public void PickUpGold()
        {
            // Increase the agent's gold count and update the grid accordingly
            GetComponent<Agent>().nbGold++;
            GridManager.RemoveFromGrids(Coords, "Gold");
            GridManager.AttachGoldToAgent(GetComponent<Agent>());
            Agent.lastAction = "PickUp";
        }

        // remove the gold from the cavern
        internal void Discard() { GridManager.RemoveFromGrids(Coords, "Gold"); }

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
            var coordWumpus = Coords;

            while (coordWumpus.x >= GameManager.Instance.gridMin.x && coordWumpus.x < GameManager.Instance.gridMax.x &&
            coordWumpus.y >= GameManager.Instance.gridMin.y && coordWumpus.y < GameManager.Instance.gridMax.y)
            {
                // Check if a Wumpus is present in the current position
                if (GameManager.Instance.AgentsMap[coordWumpus.x, coordWumpus.y].Exists(e => e.tag is "Wumpus"))
                {
                    // Shoot the Wumpus and update the grid
                    Agent.nbArrow--;
                    GridManager.RemoveFromGrids(coordWumpus, "Wumpus");
                    GridManager.RemoveFromGrids(coordWumpus, "DangerousCell");
                    GridManager.AddToGrids(coordWumpus, "DeadWumpus");
                    GridManager.AddToGrids(coordWumpus, "SafeCell");
                    Agent.lastAction = "ShootArrow";

                    // Update Agent last action to display/profile data
                    if (coordWumpus.x > Coords.x)
                        Agent.lastAction = "ShootRight";
                    else if (coordWumpus.x < Coords.x)
                        Agent.lastAction = "ShootLeft";
                    else if (coordWumpus.y > Coords.y)
                        Agent.lastAction = "ShootUp";
                    else if (coordWumpus.y < Coords.y)
                        Agent.lastAction = "ShootDown";
                    break;
                }
                coordWumpus += direction;
            }
        }
    }
}