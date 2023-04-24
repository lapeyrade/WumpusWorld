using System.Collections.Generic;
using System.Linq;
using UnityEngine;

namespace Agent
{
    public class AgentMove : MonoBehaviour
    {
        private readonly List<Vector2Int> _moves = new() { Vector2Int.right, Vector2Int.left, Vector2Int.up, Vector2Int.down };
        
        public Vector2Int SelectNextMove()
        {
            if (GetComponent<Agent>().nbGold > 0) return MoveBack();

            foreach (var move in _moves.Where(move => SafeCellUnexplored(GetComponent<Agent>().coords + move)))
            {
                return GetComponent<Agent>().coords + move;
            }

            return MoveBack();
        }

        public Vector2Int SelectRandomMove()
        {
            if (GetComponent<Agent>().nbGold > 0) return MoveBack();
            
            var randomMoves = _moves.OrderBy(_ => Random.value);

            foreach (var move in randomMoves.Where(move => SafeCellUnexplored(GetComponent<Agent>().coords + move)))
            { 
                return GetComponent<Agent>().coords + move;
            }
            return MoveBack();
        }

        private static bool SafeCellUnexplored(Vector2Int cell) =>
            GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "SafeCell") && 
            !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "VisitedCell");

        public void MoveAgent(Vector2Int newCoord)
        {
            if (newCoord == GetComponent<Agent>().coords + Vector2Int.right)
                GameManager.UpdateMoveGUI("Moving right");
            else if (newCoord == GetComponent<Agent>().coords + Vector2Int.left)
                GameManager.UpdateMoveGUI("Moving left");
            else if (newCoord == GetComponent<Agent>().coords + Vector2Int.up)
                GameManager.UpdateMoveGUI("Moving up");
            else if (newCoord == GetComponent<Agent>().coords + Vector2Int.down)
                GameManager.UpdateMoveGUI("Moving down");

            GridManager.RemoveFromGrids(GetComponent<Agent>().coords, GetComponent<Agent>().tag);
            GetComponent<Agent>().transform.position = GridManager.GetAgentMapOffset(newCoord);

            if (GetComponent<Agent>().nbGold > 0)
            { 
                GetComponent<Agent>().prefabGoldMap.transform.position = GetComponent<Agent>().prefabAgentWorld.transform.position;
                GetComponent<Agent>().prefabGoldAgent.transform.position = GetComponent<Agent>().transform.position;
            }

            GetComponent<Agent>().PastMovements.Push(newCoord);
            GetComponent<Agent>().coords = newCoord;
            GridManager.AddToGrids(GetComponent<Agent>().coords, "VisitedCell");
        }

        public Vector2Int MoveBack()
        {
            GameManager.UpdateMoveGUI("Moving back");

            if (GetComponent<Agent>().PastMovements.Count <= 1) return GetComponent<Agent>().coords; 
                GetComponent<Agent>().PastMovements.Pop();
            return GetComponent<Agent>().PastMovements.Pop();
        }
        
        public void BumpWall()
        {
            GameManager.UpdateActionGUI("Bumping wall");
            MoveAgent(MoveBack());
        }
    }
}