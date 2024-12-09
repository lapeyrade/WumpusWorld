using System.Linq;
using UnityEngine;
using GameManagement;

namespace Agent
{
    public class AgentSense : MonoBehaviour
    {
        private Agent _agent;
        private GameManager _gameManager;

        private void Awake()
        {
            _agent = GetComponent<Agent>();
            _gameManager = GameManager.Instance;
        }

        private Vector2Int Coords => _agent.coords;

        // Sense the current cell and make inferences
        public void SenseCell()
        {
            // Iterate over the elements in the current cell's map and add them to the grid
            foreach (var element in _gameManager.Map[Coords.x, Coords.y]
                         .Except(_gameManager.AgentsMap[Coords.x, Coords.y]).Select(x => x.tag))
                GridManager.AddToGrids(Coords, element);

            // Check if the current cell contains a pit or wumpus
            if (!_gameManager.Map[Coords.x, Coords.y].Exists(e => e.tag is "Pit" or "Wumpus"))
                MakeInferences();
            else
                _gameManager.SetGameOver(false);

            // Check if the agent is at the starting coordinate with one gold
            if (_agent.startCoord == Coords)
            {
                if (_agent.nbGold == 1)
                    _gameManager.SetGameOver(true);
                // Set game over if Agent has no safe cell to visit
                if (_gameManager.nbAgent is 1
                    && !_gameManager.AgentsMap[Coords.x + 1, Coords.y].Exists(e => e.tag is "SafeCell")
                    && !_gameManager.AgentsMap[Coords.x - 1, Coords.y].Exists(e => e.tag is "SafeCell")
                    && !_gameManager.AgentsMap[Coords.x, Coords.y + 1].Exists(e => e.tag is "SafeCell")
                    && !_gameManager.AgentsMap[Coords.x, Coords.y - 1].Exists(e => e.tag is "SafeCell"))
                    _gameManager.SetGameOver(false);
            }
        }

        // Make inferences about nearby cells
        private void MakeInferences()
        {
            // Check if the current cell contains breeze, stench, wumpus, or pit
            if (_gameManager.AgentsMap[Coords.x, Coords.y].Exists(x
                    => x.tag is "Breeze" or "Stench" or "Wumpus" or "Pit"))
                MarkSideCells("UnknownCell", true); // Mark side cells as unknown cells
            else
                MarkSideCells("SafeCell", false); // Mark side cells as safe cells

            // Check for dangers (wumpus and pit) in nearby cells]
            CheckCellsDanger("Wumpus", "Stench");
            CheckCellsDanger("Pit", "Breeze");
        }

        // Mark side cells as a specified element (unknown or safe)
        private void MarkSideCells(string element, bool checkDanger)
        {
            for (var i = 1; i <= 2; i++)
            {
                if (i > 1) element = "UnknownCell";

                // Mark cells in the four cardinal directions
                MarkCell(new Vector2Int(Coords.x + i, Coords.y), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x - i, Coords.y), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x, Coords.y + i), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x, Coords.y - i), element, checkDanger);

                if (i <= 1) continue;

                // Mark diagonal cells
                MarkCell(new Vector2Int(Coords.x + (i - 1), Coords.y + (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x + (i - 1), Coords.y - (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x - (i - 1), Coords.y + (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x - (i - 1), Coords.y - (i - 1)), element, checkDanger);
            }
        }

        // Mark a specific cell as the specified element
        private void MarkCell(Vector2Int cell, string element, bool checkDanger)
        {
            if (!GridManager.CellInGridLimits(cell)) return; // Check if the cell is within the grid limits

            // Check if the cell satisfies the specified conditions for marking
            if (checkDanger && _gameManager.AgentsMap[cell.x, cell.y].Exists(e =>
                    e.tag is "Wumpus" or "DeadWumpus" or "Pit")) return;

            if (element is "UnknownCell" && _gameManager.AgentsMap[cell.x, cell.y].Exists(e =>
                e.tag is "SafeCell" or "Wall" or "VisitedCell")) return;

            GridManager.AddToGrids(cell, element);
        }

        // Check nearby cells for a specific danger and its corresponding hint
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
            CheckDanger(new Vector2Int(Coords.x - 1, Coords.y - 1), danger, hint);
        }

        // Check a specific cell for a danger and its corresponding hint
        private void CheckDanger(Vector2Int cell, string danger, string hint)
        {
            // Check if the cell is within the grid limits and if it contains the specified hint
            if (!GridManager.CellInGridLimits(cell) ||
                !_gameManager.AgentsMap[cell.x, cell.y].Exists(e => e.CompareTag(hint))) return;

            // Check adjacent cells for the specified danger and mark them accordingly
            if (DangerInRightCell(cell, danger))
                AddDanger(new Vector2Int(cell.x + 1, cell.y), danger, hint);

            if (DangerInLeftCell(cell, danger))
                AddDanger(new Vector2Int(cell.x - 1, cell.y), danger, hint);

            if (DangerInUpCell(cell, danger))
                AddDanger(new Vector2Int(cell.x, cell.y + 1), danger, hint);

            if (DangerInDownCell(cell, danger))
                AddDanger(new Vector2Int(cell.x, cell.y - 1), danger, hint);
        }

        // Mark a cell as a danger and add the corresponding hint
        private void AddDanger(Vector2Int cell, string danger, string hint)
        {
            // Check if the danger is a wumpus and the cell contains a dead wumpus
            if (danger is "Wumpus" &&
                _gameManager.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "DeadWumpus")) return;

            // Add the danger element and mark the cell as a dangerous cell
            GridManager.AddToGrids(cell, danger);
            GridManager.AddToGrids(cell, "DangerousCell");

            // Add the hint to adjacent cells
            AddHint(new Vector2Int(cell.x + 1, cell.y), hint); // Right
            AddHint(new Vector2Int(cell.x - 1, cell.y), hint); // Left
            AddHint(new Vector2Int(cell.x, cell.y + 1), hint); // Up
            AddHint(new Vector2Int(cell.x, cell.y - 1), hint); // Down
        }

        // Add a hint to a specific cell
        private void AddHint(Vector2Int cell, string hint)
        {
            // Skip marking if the cell contains a wall
            if (_gameManager.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "Wall")) return;
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

        // Check if there is no danger in a specific cell
        private bool NoDangerInCell(Vector2Int cell, string danger)
        {
            if (!GridManager.CellInGridLimits(cell)) return false;

            // Check if the danger is a wumpus and the cell contains a dead wumpus
            if (danger is "Wumpus" &&
                _gameManager.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "DeadWumpus"))
                return false;

            return _gameManager.AgentsMap[cell.x, cell.y]
                       .Exists(e => e.tag is "VisitedCell" or "SafeCell" or "StartCell") &&
                   !_gameManager.AgentsMap[cell.x, cell.y].Exists(e => e.CompareTag(danger));
        }
    }
}