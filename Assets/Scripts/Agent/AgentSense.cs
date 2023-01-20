using UnityEngine;
    
namespace Agent
{
    public class AgentSense
    {
        private readonly Agent _agent;
        
        public AgentSense(Agent agent)
        {
            _agent = agent;
        }
        
        public void MakeInferences()
        {
            if (GameManager.Instance.AgentsMap[_agent.coords.x, _agent.coords.y].Exists(x
                    => x.tag is "breeze" or "stench" or "wumpus" or "pit"))
                MarkSideCells("undefined", true);
            else
                MarkSideCells("safe", false);

            CheckCellsDanger("wumpus", "stench");
            CheckCellsDanger("pit", "breeze");
        }

        private void MarkSideCells(string element, bool checkDanger)
        {
            for (int i = 1; i <= 2; i++)
            {
                if (i > 1)
                    element = "undefined";
                MarkCell(new Vector2Int(_agent.coords.x + i, _agent.coords.y), element, checkDanger);
                MarkCell(new Vector2Int(_agent.coords.x - i, _agent.coords.y), element, checkDanger);
                MarkCell(new Vector2Int(_agent.coords.x, _agent.coords.y + i), element, checkDanger);
                MarkCell(new Vector2Int(_agent.coords.x, _agent.coords.y - i), element, checkDanger);

                if (i <= 1) continue;
                MarkCell(new Vector2Int(_agent.coords.x + (i - 1), _agent.coords.y + (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(_agent.coords.x + (i - 1), _agent.coords.y - (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(_agent.coords.x - (i - 1), _agent.coords.y + (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(_agent.coords.x - (i - 1), _agent.coords.y - (i - 1)), element, checkDanger);
            }
        }

        private void MarkCell(Vector2Int cell, string element, bool checkDanger)
        {
            if (!GridManager.CellInGridLimits(cell)) return;
            if (checkDanger && GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e =>
                    e.tag is "wumpus" or "wumpusdead" or "pit")) return;
            if (element is "undefined" && GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e =>
                    e.tag is "safe" or "wall" or "visited")) return;

            GridManager.AddToGrids(cell, element);
        }

        private void CheckCellsDanger(string danger, string hint)
        {
            CheckDanger(_agent.coords, danger, hint);
            CheckDanger(new Vector2Int(_agent.coords.x + 1, _agent.coords.y), danger, hint);
            CheckDanger(new Vector2Int(_agent.coords.x - 1, _agent.coords.y), danger, hint);
            CheckDanger(new Vector2Int(_agent.coords.x, _agent.coords.y + 1), danger, hint);
            CheckDanger(new Vector2Int(_agent.coords.x, _agent.coords.y - 1), danger, hint);
            CheckDanger(new Vector2Int(_agent.coords.x + 1, _agent.coords.y + 1), danger, hint);
            CheckDanger(new Vector2Int(_agent.coords.x + 1, _agent.coords.y - 1), danger, hint);
            CheckDanger(new Vector2Int(_agent.coords.x - 1, _agent.coords.y + 1), danger, hint);
            CheckDanger(new Vector2Int(_agent.coords.x - 1, _agent.coords.y + 1), danger, hint);
        }

        private void CheckDanger(Vector2Int cell, string danger, string hint)
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

        private void AddDanger(Vector2Int cell, string danger, string hint)
        {
            if (danger is "wumpus" &&
                GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "wumpusdead"))
                return;
            GridManager.AddToGrids(cell, danger);
            GridManager.AddToGrids(cell, "danger");
            AddHint(new Vector2Int(cell.x + 1, cell.y), hint);
            AddHint(new Vector2Int(cell.x - 1, cell.y), hint);
            AddHint(new Vector2Int(cell.x, cell.y + 1), hint);
            AddHint(new Vector2Int(cell.x, cell.y - 1), hint);
        }

        private void AddHint(Vector2Int cell, string hint)
        {
            if (GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "wall")) return;
            GridManager.AddToGrids(cell, hint);
        }

        private bool DangerInRightCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
        }

        private bool DangerInLeftCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
        }

        private bool DangerInUpCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
        }

        private bool DangerInDownCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger);
        }

        private bool NoDangerInCell(Vector2Int cell, string danger)
        {
            if (!GridManager.CellInGridLimits(cell)) return false;
            if (danger == "wumpus" &&
                GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "wumpusdead"))
                return false;
            return GameManager.Instance.AgentsMap[cell.x, cell.y]
                       .Exists(e => e.tag is "visited" or "safe" or "start") &&
                   !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.CompareTag(danger));
        }
    }
}