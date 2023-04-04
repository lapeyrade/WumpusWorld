using System.Collections.Generic;
using System.Linq;
using TMPro;
using UnityEngine;

namespace Agent
{
    public class AgentMove
    {
        private readonly Agent _agent;
        private readonly List<Vector2Int> _moves;

        public AgentMove(Agent agent)
        {
            _agent = agent;
            _moves = new List<Vector2Int> { Vector2Int.right, Vector2Int.left, Vector2Int.up, Vector2Int.down };
        }
        
        public Vector2Int SelectNextMove()
        {
            if (_agent.nbGold > 0) return MoveBack();

            foreach (var move in _moves.Where(move => SafeCellUnexplored(_agent.coords + move)))
            {
                return _agent.coords + move;
            }

            return MoveBack();
        }

        public Vector2Int SelectRandomMove()
        {
            if (_agent.nbGold > 0)
                return MoveBack();
            
            var randomMoves = _moves.OrderBy(_ => Random.value);

            foreach (var move in randomMoves.Where(move => SafeCellUnexplored(_agent.coords + move)))
            { 
                return _agent.coords + move;
            }

            return MoveBack();
        }

        private static bool SafeCellUnexplored(Vector2Int cell) =>
            GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "safe") && 
            !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "visited");

        public void Move(Vector2Int newCoord)
        {
            if (newCoord == _agent.coords + Vector2Int.right)
                GameManager.Instance.UpdateMoveGUI("Moving right");
            else if (newCoord == _agent.coords + Vector2Int.left)
                GameManager.Instance.UpdateMoveGUI("Moving left");
            else if (newCoord == _agent.coords + Vector2Int.up)
                GameManager.Instance.UpdateMoveGUI("Moving up");
            else if (newCoord == _agent.coords + Vector2Int.down)
                GameManager.Instance.UpdateMoveGUI("Moving down");

            GridManager.RemoveFromGrids(_agent.coords, _agent.tag);
            _agent.transform.position = GridManager.GetAgentMapOffset(newCoord);

            if (_agent.nbGold > 0)
            {
                _agent.prefabGoldMap.transform.position = _agent.prefabAgentWorld.transform.position;
                _agent.prefabGoldAgent.transform.position = _agent.transform.position;
            }

            _agent.PastMovements.Push(newCoord);
            _agent.coords = newCoord;
            GridManager.AddToGrids(_agent.coords, "visited");
        }

        private Vector2Int MoveBack()
        {
            GameManager.Instance.UpdateMoveGUI("Moving back");

            if (_agent.PastMovements.Count <= 1) return _agent.coords;
            _agent.PastMovements.Pop();
            return _agent.PastMovements.Pop();
        }
        
        public void BumpWall()
        {
            GameManager.Instance.UpdateActionGUI("Bumping wall");;
            Move(MoveBack());
        }
    }
}