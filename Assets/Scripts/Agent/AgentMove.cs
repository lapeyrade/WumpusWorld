using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using Prolog;
using GameManagement;

namespace Agent
{
    public class AgentMove : MonoBehaviour
    {
        private readonly List<Vector2Int> _moves =
            new() { Vector2Int.right, Vector2Int.left, Vector2Int.up, Vector2Int.down };
        private Agent Agent => GetComponent<Agent>();
        private Vector2Int Coords => Agent.coords;

        public void MoveCell()
        {
            if (Input.GetKeyDown(KeyCode.RightArrow))
                MoveAgent(Coords + Vector2Int.right);
            else if (Input.GetKeyDown(KeyCode.LeftArrow))
                MoveAgent(Coords + Vector2Int.left);
            else if (Input.GetKeyDown(KeyCode.UpArrow))
                MoveAgent(Coords + Vector2Int.up);
            else if (Input.GetKeyDown(KeyCode.DownArrow))
                MoveAgent(Coords + Vector2Int.down);
            else if (Input.GetKeyDown(KeyCode.Space) || GameManager.Instance.isModeAuto)
                MoveAgent(SelectNextMove());
            else if (Input.GetKeyDown(KeyCode.Return))
                MoveAgent(SelectRandomMove());
        }

        // Selects the next move based on the agent's state and surroundings
        private Vector2Int SelectNextMove()
        {
            if (Agent.nbGold > 0)
                return MoveBack();

            foreach (var move in _moves.Where(move => SafeCellUnexplored(Coords + move)))
                return Coords + move;

            return MoveBack();
        }

        // Selects a random move based on the agent's state and surroundings
        private Vector2Int SelectRandomMove()
        {
            if (Agent.nbGold > 0)
                return MoveBack();

            var randomMoves = _moves.OrderBy(_ => Random.value);

            foreach (var move in randomMoves.Where(move => SafeCellUnexplored(Coords + move)))
                return Coords + move;

            return MoveBack();
        }

        // Checks if the specified cell is a safe, unexplored cell
        private static bool SafeCellUnexplored(Vector2Int cell) =>
            GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "SafeCell") &&
            !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "VisitedCell");

        // Moves the agent to the specified new coordinate
        public void MoveAgent(Vector2Int newCoord)
        {
            if (GameManager.Instance.AgentsMap[newCoord.x, newCoord.y].Exists(e => e.tag is "VisitedCell"))
                Agent.lastAction = "MoveBack";
            else if (newCoord.x > Coords.x)
                Agent.lastAction = "MoveRight";
            else if (newCoord.x < Coords.x)
                Agent.lastAction = "MoveLeft";
            else if (newCoord.y > Coords.y)
                Agent.lastAction = "MoveUp";
            else if (newCoord.y < Coords.y)
                Agent.lastAction = "MoveDown";

            GridManager.RemoveFromGrids(Coords, tag);
            GameManager.Instance.GetComponent<PrologInterface>().QueryText += $", retract(data_concept([{Agent.name}, [{Agent.coords.x}, {Agent.coords.y}]], {Agent.tag.ToLower()})), retract({Agent.tag.ToLower()}([{Agent.name}, [{Agent.coords.x}, {Agent.coords.y}]]))";
            
            Agent.transform.position = GridManager.GetAgentMapOffset(newCoord);
            

            if (Agent.nbGold > 0)
            {
                Agent.prefabGoldMap.transform.position = Agent.prefabAgentWorld.transform.position;
                Agent.prefabGoldAgent.transform.position = Agent.transform.position;
            }

            Agent.PastMovements.Push(newCoord);
            Agent.coords = newCoord;

            GameManager.Instance.GetComponent<PrologInterface>().QueryText += $", assertz(data_concept([{Agent.name}, [{Agent.coords.x}, {Agent.coords.y}]], {Agent.tag.ToLower()})), assertz({Agent.tag.ToLower()}([{Agent.name}, [{Agent.coords.x}, {Agent.coords.y}]]))";
            GridManager.AddToGrids(Coords, "VisitedCell");
        }

        // Moves the agent back to the previous position
        public Vector2Int MoveBack()
        {
            if (Agent.PastMovements.Count <= 1) return Coords;

            Agent.PastMovements.Pop();
            return Agent.PastMovements.Pop();
        }

        // Moves the agent back when it bumps into a wall
        public void BumpWall() => MoveAgent(MoveBack());
    }
}