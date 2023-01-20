using System.Collections.Generic;
using System.Linq;
using UnityEngine;

namespace Agent
{
    public class AgentMove
    {
        private readonly Agent _agent;

        public AgentMove(Agent agent)
        {
            _agent = agent;
        }
        
        public Vector2Int SelectNextMove()
        {
            if (_agent.nbGold > 0)
                return MoveBack();

            var moves = new List<Vector2Int>
            {
                new(_agent.coords.x + 1, _agent.coords.y), new(_agent.coords.x - 1, _agent.coords.y),
                new(_agent.coords.x, _agent.coords.y + 1), new(_agent.coords.x, _agent.coords.y - 1)
            };

            foreach (var move in moves.Where(SafeCellUnexplored))
            {
                return move;
            }

            return MoveBack();
        }

        public Vector2Int SelectRandomMove()
        {
            if (_agent.nbGold > 0)
                return MoveBack();

            var randomMoves = new List<Vector2Int>
            {
                new(_agent.coords.x + 1, _agent.coords.y), new(_agent.coords.x - 1, _agent.coords.y),
                new(_agent.coords.x, _agent.coords.y + 1), new(_agent.coords.x, _agent.coords.y - 1)
            }.OrderBy(_ => Random.value);

            foreach (var move in randomMoves)
            {
                if (SafeCellUnexplored(move)) return move;
            }

            return MoveBack();
        }

        private bool SafeCellUnexplored(Vector2Int cell)
        {
            return GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "safe") &&
                   !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "visited");
        }

        public void Move(Vector2Int newCoord)
        {
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
            Debug.Log("MoveBack");
            if (_agent.PastMovements.Count <= 1) return _agent.coords;
            _agent.PastMovements.Pop();
            return _agent.PastMovements.Pop();
        }
        
        public void BumpWall()
        {
            Move(MoveBack());
        }
    }
}