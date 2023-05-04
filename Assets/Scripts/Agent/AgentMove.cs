using System.Collections.Generic;
using System.Linq;
using UnityEngine;

namespace Agent
{
    public class AgentMove : MonoBehaviour
    {
        private readonly List<Vector2Int> _moves = new() { Vector2Int.right, Vector2Int.left, Vector2Int.up, Vector2Int.down };
        private Agent Agent => GetComponent<Agent>();
        private Vector2Int Coords
        {
            get => Agent.coords;
            set => Agent.coords = value;
        }

        public void MoveCell()
        {
            if (Input.GetKeyDown("right"))
                MoveAgent(new Vector2Int(Coords.x + 1, Coords.y));
            else if (Input.GetKeyDown("left"))
                MoveAgent(new Vector2Int(Coords.x - 1, Coords.y));
            else if (Input.GetKeyDown("up"))
                MoveAgent(new Vector2Int(Coords.x, Coords.y + 1));
            else if (Input.GetKeyDown("down"))
                MoveAgent(new Vector2Int(Coords.x, Coords.y - 1));
            else if (Input.GetKeyDown("space") || GameManager.Instance.isModeAuto) // IA
                MoveAgent(SelectNextMove());
            else if (Input.GetKeyDown("return")) // IA Random
                MoveAgent(SelectRandomMove());
        }
        
        private Vector2Int SelectNextMove()
        {
            if (Agent.nbGold > 0) return MoveBack();

            foreach (var move in _moves.Where(move => SafeCellUnexplored(Coords + move)))
                return Coords + move;

            return MoveBack();
        }

        private Vector2Int SelectRandomMove()
        {
            if (Agent.nbGold > 0) return MoveBack();
            
            var randomMoves = _moves.OrderBy(_ => Random.value);

            foreach (var move in randomMoves.Where(move => SafeCellUnexplored(Coords + move)))
                return Coords + move;
            
            return MoveBack();
        }

        private static bool SafeCellUnexplored(Vector2Int cell) =>
            GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "SafeCell") && 
            !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "VisitedCell");

        public void MoveAgent(Vector2Int newCoord)
        {
            if (newCoord == Coords + Vector2Int.right)
                GameManager.UpdateMoveGUI("Moving right");
            else if (newCoord == Coords + Vector2Int.left)
                GameManager.UpdateMoveGUI("Moving left");
            else if (newCoord == Coords + Vector2Int.up)
                GameManager.UpdateMoveGUI("Moving up");
            else if (newCoord == Coords + Vector2Int.down)
                GameManager.UpdateMoveGUI("Moving down");

            GridManager.RemoveFromGrids(Coords, tag);
            Agent.transform.position = GridManager.GetAgentMapOffset(newCoord);

            if (Agent.nbGold > 0)
            { 
                Agent.prefabGoldMap.transform.position = Agent.prefabAgentWorld.transform.position;
                Agent.prefabGoldAgent.transform.position = Agent.transform.position;
            }

            Agent.PastMovements.Push(newCoord);
            Coords = newCoord;
            GridManager.AddToGrids(Coords, "VisitedCell");
        }

        public Vector2Int MoveBack()
        {
            GameManager.UpdateMoveGUI("Moving back");

            if (Agent.PastMovements.Count <= 1) return Coords; 
                Agent.PastMovements.Pop();
            return Agent.PastMovements.Pop();
        }
        
        public void BumpWall()
        {
            GameManager.UpdateActionGUI("Bumping wall");
            MoveAgent(MoveBack());
        }
    }
}