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
        private Agent _agent;
        private PrologInterface _prologInterface;

        private void Awake()
        {
            _agent = GetComponent<Agent>();
            _prologInterface = GameManager.Instance.GetComponent<PrologInterface>();
        }

        private Vector2Int Coords => _agent.coords;

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
            if (_agent.nbGold > 0)
                return MoveBack();

            foreach (var move in _moves.Where(move => SafeCellUnexplored(Coords + move)))
                return Coords + move;

            return MoveBack();
        }

        // Selects a random move based on the agent's state and surroundings
        private Vector2Int SelectRandomMove()
        {
            if (_agent.nbGold > 0)
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
                _agent.lastAction = "MoveBack";
            else if (newCoord.x > Coords.x)
                _agent.lastAction = "MoveRight";
            else if (newCoord.x < Coords.x)
                _agent.lastAction = "MoveLeft";
            else if (newCoord.y > Coords.y)
                _agent.lastAction = "MoveUp";
            else if (newCoord.y < Coords.y)
                _agent.lastAction = "MoveDown";

            GridManager.RemoveFromGrids(Coords, tag);
            _prologInterface.QueryText += $", retract(data_concept([{_agent.name}, [{_agent.coords.x}, {_agent.coords.y}]], {_agent.tag.ToLower()})), retract({_agent.tag.ToLower()}([{_agent.name}, [{_agent.coords.x}, {_agent.coords.y}]]))";
            
            _agent.transform.position = GridManager.GetAgentMapOffset(newCoord);
            

            if (_agent.nbGold > 0)
            {
                _agent.prefabGoldMap.transform.position = _agent.prefabAgentWorld.transform.position;
                _agent.prefabGoldAgent.transform.position = _agent.transform.position;
            }

            _agent.PastMovements.Push(newCoord);
            _agent.coords = newCoord;

            _prologInterface.QueryText += $", assertz(data_concept([{_agent.name}, [{_agent.coords.x}, {_agent.coords.y}]], {_agent.tag.ToLower()})), assertz({_agent.tag.ToLower()}([{_agent.name}, [{_agent.coords.x}, {_agent.coords.y}]]))";
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