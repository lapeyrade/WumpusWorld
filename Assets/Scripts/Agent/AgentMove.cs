using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using Prolog;
using GameManagement;

namespace Agent
{
    public class AgentMove : MonoBehaviour
    {
        // List of possible movement directions (right, left, up, down)
        private readonly List<Vector2Int> _moves =
            new() { Vector2Int.right, Vector2Int.left, Vector2Int.up, Vector2Int.down };
        
        // Core components
        private Agent _agent;
        private PrologInterface _prologInterface;

        private void Awake()
        {
            _agent = GetComponent<Agent>();
            _prologInterface = GameManager.Instance.GetComponent<PrologInterface>();
        }

        // Shorthand property for current agent coordinates
        private Vector2Int Coords => _agent.coords;

        // Handle movement input and automatic movement
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
                MoveAgent(SelectNextMove());      // Auto mode: Choose next strategic move
            else if (Input.GetKeyDown(KeyCode.Return))
                MoveAgent(SelectRandomMove());    // Random movement mode
        }

        // Selects the next move based on the agent's state and surroundings
        private Vector2Int SelectNextMove()
        {
            // Return to base if carrying gold
            if (_agent.nbGold > 0)
                return MoveBack();

            // Look for safe, unexplored cells to move to
            foreach (var move in _moves.Where(move => SafeCellUnexplored(Coords + move)))
                return Coords + move;

            // If no safe unexplored cells found, move back
            return MoveBack();
        }

        // Selects a random move based on the agent's state and surroundings
        private Vector2Int SelectRandomMove()
        {
            // Return to base if carrying gold
            if (_agent.nbGold > 0)
                return MoveBack();

            // Randomly shuffle possible moves
            var randomMoves = _moves.OrderBy(_ => Random.value);

            // Look for safe, unexplored cells to move to
            foreach (var move in randomMoves.Where(move => SafeCellUnexplored(Coords + move)))
                return Coords + move;

            // If no safe unexplored cells found, move back
            return MoveBack();
        }

        // Checks if the specified cell is a safe, unexplored cell
        private static bool SafeCellUnexplored(Vector2Int cell) =>
            GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "SafeCell") &&
            !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "VisitedCell");

        // Moves the agent to the specified new coordinate
        public void MoveAgent(Vector2Int newCoord)
        {
            // Update agent's last action based on movement direction
            if (GameManager.Instance.AgentsMap[newCoord.x, newCoord.y].Exists(e => e.tag is "VisitedCell"))
                _agent.lastAction = "MoveBack";
            else if (newCoord.x > Coords.x)
                _agent.lastAction = "MoveRight";
            else if (newCoord.x < Coords.x)
                _agent.lastAction = "MoveLeft";
            else if (newCoord.y > Coords.y)
                _agent.lastAction = "MoveUp";
            else if (newCoord.y < Coords.y)
                _agent.lastAction = "MoveDown";

            // Update Prolog knowledge base with agent's movement
            GridManager.RemoveFromGrids(Coords, tag);
            
            var agentCoords = $"[{_agent.coords.x}, {_agent.coords.y}]";
            var agentName = _agent.name;
            var agentTag = _agent.tag.ToLower();
            
            _prologInterface.QueryText += 
                $", retract(data_concept([{agentName}, {agentCoords}], {agentTag})), " +
                $"retract({agentTag}([{agentName}, {agentCoords}]))";
            
            // Update agent's position and visuals
            _agent.transform.position = GridManager.GetAgentMapOffset(newCoord);
            
            // Update gold position if agent is carrying gold
            if (_agent.nbGold > 0)
            {
                _agent.prefabGoldMap.transform.position = 
                    _agent.prefabAgentWorld.transform.position;
                _agent.prefabGoldAgent.transform.position = 
                    _agent.transform.position;
            }

            // Record movement and update coordinates
            _agent.PastMovements.Push(newCoord);
            _agent.coords = newCoord;

            // Update Prolog knowledge base with new position
            agentCoords = $"[{_agent.coords.x}, {_agent.coords.y}]";
            _prologInterface.QueryText +=
                $", assertz(data_concept([{agentName}, {agentCoords}], {agentTag})), " +
                $"assertz({agentTag}([{agentName}, {agentCoords}]))";
                
            GridManager.AddToGrids(Coords, "VisitedCell");
        }

        // Moves the agent back to the previous position
        public Vector2Int MoveBack()
        {
            if (_agent.PastMovements.Count <= 1) return Coords;

            _agent.PastMovements.Pop();
            return _agent.PastMovements.Pop();
        }

        // Moves the agent back when it bumps into a wall
        public void BumpWall() => MoveAgent(MoveBack());
    }
}