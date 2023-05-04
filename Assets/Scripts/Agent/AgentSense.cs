using System.Linq;
using UnityEngine;

namespace Agent
{
    public class AgentSense : MonoBehaviour
    {
        private Agent Agent => GetComponent<Agent>();
        private Vector2Int Coords => Agent.coords;
        
        public void SenseCell()
        {
            if (Agent.startCoord == Coords && Agent.nbGold == 1)
                GameManager.Instance.SetGameOver($"{name} Won!", false);

            foreach (var element in GameManager.Instance.Map[Coords.x, Coords.y]
                         .Except(GameManager.Instance.AgentsMap[Coords.x, Coords.y]).Select(x => x.tag))
                GridManager.AddToGrids(Coords, element);

            if (!GameManager.Instance.Map[Coords.x, Coords.y].Exists(e => e.tag is "Pit" or "Wumpus"))
                GetComponent<AgentSense>().MakeInferences();
            else GameManager.Instance.SetGameOver($"{name} Lost!", false);
        }

        private void MakeInferences()
        {
            if (GameManager.Instance.AgentsMap[Coords.x, Coords.y].Exists(x
                    => x.tag is "Breeze" or "Stench" or "Wumpus" or "Pit"))
                MarkSideCells("UnknownCell", true);
            else
                MarkSideCells("SafeCell", false);

            CheckCellsDanger("Wumpus", "Stench");
            CheckCellsDanger("Pit", "Breeze");
        }

        private void MarkSideCells(string element, bool checkDanger)
        {
            for (var i = 1; i <= 2; i++)
            {
                if (i > 1) element = "UnknownCell";
                MarkCell(new Vector2Int(Coords.x + i, Coords.y), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x - i, Coords.y), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x, Coords.y + i), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x, Coords.y - i), element, checkDanger);

                if (i <= 1) continue;
                MarkCell(new Vector2Int(Coords.x + (i - 1), Coords.y + (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x + (i - 1), Coords.y - (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x - (i - 1), Coords.y + (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x - (i - 1), Coords.y - (i - 1)), element, checkDanger);
            }
        }

        private static void MarkCell(Vector2Int cell, string element, bool checkDanger)
        {
            if (!GridManager.CellInGridLimits(cell)) return;
            if (checkDanger && GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e =>
                    e.tag is "Wumpus" or "DeadWumpus" or "Pit")) return;
            if (element is "UnknownCell" && GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e =>
                    e.tag is "SafeCell" or "Wall" or "VisitedCell")) return;

            GridManager.AddToGrids(cell, element);
        }

        private void CheckCellsDanger(string danger, string hint)
        {
            CheckDanger(Coords, danger, hint);
            CheckDanger(new Vector2Int(Coords.x + 1, Coords.y), danger, hint);
            CheckDanger(new Vector2Int(Coords.x - 1, Coords.y), danger, hint);
            CheckDanger(new Vector2Int(Coords.x, Coords.y + 1), danger, hint);
            CheckDanger(new Vector2Int(Coords.x, Coords.y - 1), danger, hint);
            CheckDanger(new Vector2Int(Coords.x + 1, Coords.y + 1), danger, hint);
            CheckDanger(new Vector2Int(Coords.x + 1, Coords.y - 1), danger, hint);
            CheckDanger(new Vector2Int(Coords.x - 1, Coords.y + 1), danger, hint);
            CheckDanger(new Vector2Int(Coords.x - 1, Coords.y + 1), danger, hint);
        }

        private static void CheckDanger(Vector2Int cell, string danger, string hint)
        {
            if (!GridManager.CellInGridLimits(cell) ||
                !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.CompareTag(hint))) return;
            if (DangerInRightCell(cell, danger))
                AddDanger(new Vector2Int(cell.x + 1, cell.y), danger, hint);
            if (DangerInLeftCell(cell, danger))
                AddDanger(new Vector2Int(cell.x - 1, cell.y), danger, hint);
            if (DangerInUpCell(cell, danger))
                AddDanger(new Vector2Int(cell.x, cell.y + 1), danger, hint);
            if (DangerInDownCell(cell, danger))
                AddDanger(new Vector2Int(cell.x, cell.y - 1), danger, hint);
        }

        private static void AddDanger(Vector2Int cell, string danger, string hint)
        {
            if (danger is "Wumpus" &&
                GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "DeadWumpus"))
                return;
            GridManager.AddToGrids(cell, danger);
            GridManager.AddToGrids(cell, "DangerousCell");
            AddHint(new Vector2Int(cell.x + 1, cell.y), hint);
            AddHint(new Vector2Int(cell.x - 1, cell.y), hint);
            AddHint(new Vector2Int(cell.x, cell.y + 1), hint);
            AddHint(new Vector2Int(cell.x, cell.y - 1), hint);
        }

        private static void AddHint(Vector2Int cell, string hint)
        {
            if (GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "Wall")) return;
            GridManager.AddToGrids(cell, hint);
        }

        private static bool DangerInRightCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
        }

        private static bool DangerInLeftCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
        }

        private static bool DangerInUpCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
        }

        private static bool DangerInDownCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger);
        }

        private static bool NoDangerInCell(Vector2Int cell, string danger)
        {
            if (!GridManager.CellInGridLimits(cell)) return false;
            if (danger == "Wumpus" &&
                GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "DeadWumpus"))
                return false;
            return GameManager.Instance.AgentsMap[cell.x, cell.y]
                       .Exists(e => e.tag is "VisitedCell" or "SafeCell" or "StartCell") &&
                   !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.CompareTag(danger));
        }
    }
}