using System.Collections.Generic;
using UnityEngine;
using Random = UnityEngine.Random;

namespace GameManagement
{
    // GridBuilder class is responsible for generating the game grid with all elements.
    public class GridBuilder : MonoBehaviour
    {
        // Core components
        private GameManager _gameManager;
        private Agent.Agent[] _agentComponents;

        // Build the game grid with all elements.
        public void Awake()
        {
            _gameManager = GameManager.Instance;
            ValidateGridSize();      // Ensure grid can contain all elements
            GenerateCell();          // Create base grid cells
            GenerateWall();          // Create boundary walls
            
            // Generate game elements in specific order to ensure proper placement
            GenerateElement("Human", _gameManager.nbAgent,     // Place agents first
                new List<string> { "StartCell", "Wall" }, false);
            
            GenerateElement("Gold", _gameManager.nbGold,       // Place gold pieces
                new List<string> { "StartCell", "Wall", "Gold" });
            
            GenerateElement("Wumpus", _gameManager.nbWumpus,   // Place Wumpuses and their stenches
                new List<string> { "StartCell", "Wall", "Gold", "Wumpus" }, true, "Stench");
            
            GenerateElement("Pit", _gameManager.nbPit,         // Place pits and their breezes
                new List<string> { "StartCell", "Wall", "Gold", "Wumpus", "Pit" }, true, "Breeze");
        }

        // Validate if the grid size is sufficient for all elements
        private void ValidateGridSize()
        {
            int totalElements = _gameManager.nbPit + _gameManager.nbWumpus + 
                               _gameManager.nbGold + _gameManager.nbAgent;
            int gridSize = _gameManager.gridMax.x * _gameManager.gridMax.y;
            
            if (totalElements <= gridSize) return;
            
            Debug.LogError("Map too small, can't contain all the elements.");
            Application.Quit();
            UnityEditor.EditorApplication.isPlaying = false;
        }

        // Generate the base grid of cells
        private void GenerateCell()
        {
            for (var i = _gameManager.gridMin.x; i < _gameManager.gridMax.x; i++)
            {
                for (var j = _gameManager.gridMin.y; j < _gameManager.gridMax.y; j++)
                {
                    _gameManager.AgentsMap[i, j] = new List<GameObject>();
                    _gameManager.Map[i, j] = new List<GameObject>();
                    GridManager.AddToGrids(new Vector2Int(i, j), "Cell");
                }
            }
        }

        // Generate walls around the grid perimeter
        private void GenerateWall()
        {
            // Right wall
            for (var i = _gameManager.gridMin.y; i < _gameManager.gridMax.y; i++)
                GridManager.AddToGrids(new Vector2Int(_gameManager.gridMax.x - 1, i), "Wall");

            // Left wall
            for (var i = _gameManager.gridMin.y; i < _gameManager.gridMax.y; i++)
                GridManager.AddToGrids(new Vector2Int(_gameManager.gridMin.x, i), "Wall");

            // Top wall
            for (var i = _gameManager.gridMin.y + 1; i < _gameManager.gridMax.x - 1; i++)
                GridManager.AddToGrids(new Vector2Int(i, _gameManager.gridMax.y - 1), "Wall");

            // Bottom wall
            for (var i = _gameManager.gridMin.y + 1; i < _gameManager.gridMax.x - 1; i++)
                GridManager.AddToGrids(new Vector2Int(i, _gameManager.gridMin.y), "Wall");
        }

        // Generate game elements with specified parameters
        // elem: Type of element to generate
        // count: Number of elements to generate
        // occupiedTags: List of tags that the element cannot be placed on
        // genAround: Whether to generate additional elements around this element
        // aroundElem: Type of element to generate around (e.g., Stench around Wumpus)
        private void GenerateElement(string elem, int count, ICollection<string> occupiedTags, 
                                   bool genAround = false, string aroundElem = "")
        {
            _agentComponents = new Agent.Agent[count];
            for (var i = 0; i < count; i++)
            {
                Vector2Int coords;
                bool isValid;

                // Find valid placement location
                do
                {
                    coords = new Vector2Int(
                        Random.Range(_gameManager.gridMin.x + 1, _gameManager.gridMax.x - 1),
                        Random.Range(_gameManager.gridMin.y + 1, _gameManager.gridMax.y - 1));

                    isValid = !_gameManager.Map[coords.x, coords.y]
                        .Exists(x => occupiedTags.Contains(x.tag));

                    // Special placement rules for dangerous elements
                    if (isValid && (elem == "Pit" || elem == "Wumpus"))
                    {
                        var adjacentCoords = new[]
                        {
                            new Vector2Int(coords.x + 1, coords.y),
                            new Vector2Int(coords.x - 1, coords.y),
                            new Vector2Int(coords.x, coords.y + 1),
                            new Vector2Int(coords.x, coords.y - 1)
                        };

                        // Prevent placement next to starting positions
                        foreach (var adjCoord in adjacentCoords)
                        {
                            if (_gameManager.Map[adjCoord.x, adjCoord.y]
                                .Exists(x => x.CompareTag("StartCell")))
                            {
                                isValid = false;
                                break;
                            }
                        }
                    }
                } while (!isValid);

                // Special handling for human agents
                if (elem == "Human")
                {
                    var agent = Instantiate(Resources.Load("Human"), transform) as GameObject;
                    if (agent == null) continue;
                    
                    _agentComponents[i] = agent.GetComponent<Agent.Agent>();
                    _agentComponents[i].Init(i, coords, _gameManager.nbWumpus);

                    GridManager.AddToGrids(coords, "StartCell");
                    _gameManager.agents.Add(agent);
                }

                // Add element to grid
                GridManager.AddToGrids(coords, elem);

                // Generate surrounding elements if needed
                if (genAround)
                    GenerateAroundCell(coords, aroundElem);
            }
        }

        // Generate elements around a specific cell (e.g., Stench around Wumpus)
        private static void GenerateAroundCell(Vector2Int coords, string element)
        {
            // Add elements to adjacent cells (non-diagonal)
            Generate(new Vector2Int(coords.x + 1, coords.y), element);
            Generate(new Vector2Int(coords.x - 1, coords.y), element);
            Generate(new Vector2Int(coords.x, coords.y + 1), element);
            Generate(new Vector2Int(coords.x, coords.y - 1), element);

            // Local function to handle element generation
            void Generate(Vector2Int newCoords, string elem)
            {
                if (!GameManager.Instance.Map[newCoords.x, newCoords.y]
                        .Exists(x => x.CompareTag(elem)) &&
                    !GameManager.Instance.Map[newCoords.x, newCoords.y]
                        .Exists(x => x.tag is "Wall"))
                    GridManager.AddToGrids(newCoords, elem);
            }
        }
    }
}