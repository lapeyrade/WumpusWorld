using System.Linq;
using UnityEngine;
using GameManagement;

namespace Agent
{
    public class AgentSense : MonoBehaviour
    {
        // Core components
        private Agent _agent;
        private GameManager _gameManager;

        private void Awake()
        {
            _agent = GetComponent<Agent>();
            _gameManager = GameManager.Instance;
        }

        // Shorthand property for agent coordinates
        private Vector2Int Coords => _agent.coords;

        // Update agent's knowledge of the current cell and make logical inferences
        public void SenseCell()
        {
            // Add all elements from the current cell to the agent's knowledge grid
            // (excluding other agents)
            foreach (var element in _gameManager.Map[Coords.x, Coords.y]
                         .Except(_gameManager.AgentsMap[Coords.x, Coords.y]).Select(x => x.tag))
                GridManager.AddToGrids(Coords, element);

            // Check if the current cell is safe (no pit or wumpus)
            if (!_gameManager.Map[Coords.x, Coords.y].Exists(e => e.tag is "Pit" or "Wumpus"))
                MakeInferences();
            else
                _gameManager.SetGameOver(false);  // Game over if agent enters dangerous cell

            // Check win/lose conditions at starting position
            if (_agent.startCoord == Coords)
            {
                // Win condition: Agent returns to start with gold
                if (_agent.nbGold == 1)
                    _gameManager.SetGameOver(true);
                
                // Lose condition: No safe cells to visit from starting position
                if (_gameManager.nbAgent is 1
                    && !_gameManager.AgentsMap[Coords.x + 1, Coords.y].Exists(e => e.tag is "SafeCell")
                    && !_gameManager.AgentsMap[Coords.x - 1, Coords.y].Exists(e => e.tag is "SafeCell")
                    && !_gameManager.AgentsMap[Coords.x, Coords.y + 1].Exists(e => e.tag is "SafeCell")
                    && !_gameManager.AgentsMap[Coords.x, Coords.y - 1].Exists(e => e.tag is "SafeCell"))
                    _gameManager.SetGameOver(false);
            }
        }

        // Make logical inferences about nearby cells based on current cell's contents
        private void MakeInferences()
        {
            // If current cell has danger indicators, mark adjacent cells as unknown
            if (_gameManager.AgentsMap[Coords.x, Coords.y].Exists(x
                    => x.tag is "Breeze" or "Stench" or "Wumpus" or "Pit"))
                MarkSideCells("UnknownCell", true);
            else
                MarkSideCells("SafeCell", false);  // Otherwise mark them as safe

            // Check for specific dangers in nearby cells
            CheckCellsDanger("Wumpus", "Stench");  // Check for Wumpus based on stench
            CheckCellsDanger("Pit", "Breeze");     // Check for Pits based on breeze
        }

        // Mark cells around the agent with specified element type
        private void MarkSideCells(string element, bool checkDanger)
        {
            // Mark cells in increasing radius (1-2)
            for (var i = 1; i <= 2; i++)
            {
                if (i > 1) element = "UnknownCell";  // Outer radius always marked as unknown

                // Mark orthogonal cells
                MarkCell(new Vector2Int(Coords.x + i, Coords.y), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x - i, Coords.y), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x, Coords.y + i), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x, Coords.y - i), element, checkDanger);

                if (i <= 1) continue;

                // Mark diagonal cells in outer radius
                MarkCell(new Vector2Int(Coords.x + (i - 1), Coords.y + (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x + (i - 1), Coords.y - (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x - (i - 1), Coords.y + (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(Coords.x - (i - 1), Coords.y - (i - 1)), element, checkDanger);
            }
        }

        // Mark a specific cell with the given element if conditions are met
        private void MarkCell(Vector2Int cell, string element, bool checkDanger)
        {
            if (!GridManager.CellInGridLimits(cell)) return;  // Skip if cell is outside grid

            // Skip if cell contains danger when checking for dangers
            if (checkDanger && _gameManager.AgentsMap[cell.x, cell.y].Exists(e =>
                    e.tag is "Wumpus" or "DeadWumpus" or "Pit")) return;

            // Skip if trying to mark as unknown but cell is already known
            if (element is "UnknownCell" && _gameManager.AgentsMap[cell.x, cell.y].Exists(e =>
                e.tag is "SafeCell" or "Wall" or "VisitedCell")) return;

            GridManager.AddToGrids(cell, element);
        }

        // Check cells for specific dangers based on their indicators
        private void CheckCellsDanger(string danger, string hint)
        {
            // Check current and surrounding cells for danger patterns
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

        // Check a specific cell for danger indicators and mark adjacent cells accordingly
        private void CheckDanger(Vector2Int cell, string danger, string hint)
        {
            // Skip if cell is outside grid or doesn't contain the hint
            if (!GridManager.CellInGridLimits(cell) ||
                !_gameManager.AgentsMap[cell.x, cell.y].Exists(e => e.CompareTag(hint))) return;

            // Check each direction for potential danger location
            if (DangerInRightCell(cell, danger))
                AddDanger(new Vector2Int(cell.x + 1, cell.y), danger, hint);

            if (DangerInLeftCell(cell, danger))
                AddDanger(new Vector2Int(cell.x - 1, cell.y), danger, hint);

            if (DangerInUpCell(cell, danger))
                AddDanger(new Vector2Int(cell.x, cell.y + 1), danger, hint);

            if (DangerInDownCell(cell, danger))
                AddDanger(new Vector2Int(cell.x, cell.y - 1), danger, hint);
        }

        // Add danger markers and hints to a cell and its surroundings
        private void AddDanger(Vector2Int cell, string danger, string hint)
        {
            // Skip if trying to mark Wumpus in cell with dead Wumpus
            if (danger is "Wumpus" &&
                _gameManager.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "DeadWumpus")) return;

            // Mark cell as dangerous
            GridManager.AddToGrids(cell, danger);
            GridManager.AddToGrids(cell, "DangerousCell");

            // Add danger hints to adjacent cells
            AddHint(new Vector2Int(cell.x + 1, cell.y), hint);
            AddHint(new Vector2Int(cell.x - 1, cell.y), hint);
            AddHint(new Vector2Int(cell.x, cell.y + 1), hint);
            AddHint(new Vector2Int(cell.x, cell.y - 1), hint);
        }

        // Add a hint to a cell if it's not a wall
        private void AddHint(Vector2Int cell, string hint)
        {
            if (_gameManager.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "Wall")) return;
            GridManager.AddToGrids(cell, hint);
        }

        // Helper methods to check for danger in different directions
        private bool DangerInRightCell(Vector2Int cell, string danger) =>
            NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
            NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger) &&
            NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);

        private bool DangerInLeftCell(Vector2Int cell, string danger) =>
            NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
            NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger) &&
            NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);

        private bool DangerInUpCell(Vector2Int cell, string danger) =>
            NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
            NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
            NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);

        private bool DangerInDownCell(Vector2Int cell, string danger) =>
            NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
            NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
            NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger);

        // Check if a cell is safe from a specific danger
        private bool NoDangerInCell(Vector2Int cell, string danger)
        {
            if (!GridManager.CellInGridLimits(cell)) return false;

            // Consider dead Wumpus when checking for Wumpus danger
            if (danger is "Wumpus" &&
                _gameManager.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "DeadWumpus"))
                return false;

            return _gameManager.AgentsMap[cell.x, cell.y]
                       .Exists(e => e.tag is "VisitedCell" or "SafeCell" or "StartCell") &&
                   !_gameManager.AgentsMap[cell.x, cell.y].Exists(e => e.CompareTag(danger));
        }
    }
}