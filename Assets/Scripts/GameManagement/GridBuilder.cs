using System.Collections.Generic;
using UnityEngine;
using Random = UnityEngine.Random;

namespace GameManagement
{
    // GridBuilder class is responsible for generating the game grid with all elements.
    public class GridBuilder : MonoBehaviour
    {
        private GameManager _gameManager;
        private Agent.Agent[] _agentComponents;

        // Build the game grid with all elements.
        public void Awake()
        {
            _gameManager = GameManager.Instance;
            ValidateGridSize(); // Check if the grid size is valid.
            GenerateCell(); // Generate cells in the grid.
            GenerateWall(); // Generate walls around the grid.
            GenerateElement("Human", _gameManager.nbAgent, // Generate human agents on the grid.
             new List<string> { "StartCell", "Wall" }, false);
            GenerateElement("Gold", _gameManager.nbGold, // Generate gold on the grid.
             new List<string> { "StartCell", "Wall", "Gold" });
            GenerateElement("Wumpus", _gameManager.nbWumpus, // Generate Wumpus on the grid.
             new List<string> { "StartCell", "Wall", "Gold", "Wumpus" }, true, "Stench");
            GenerateElement("Pit", _gameManager.nbPit, // Generate pits on the grid.
             new List<string> { "StartCell", "Wall", "Gold", "Wumpus", "Pit" }, true, "Breeze");
        }

        // Validate if the grid size is enough to contain all elements.
        private void ValidateGridSize()
        {
            if (_gameManager.nbPit + _gameManager.nbWumpus + _gameManager.nbGold +
                _gameManager.nbAgent <= _gameManager.gridMax.x * _gameManager.gridMax.y) return;
            Debug.LogError("Map too small, can't contain all the elements.");
            Application.Quit();
            UnityEditor.EditorApplication.isPlaying = false;
        }

        // Generate cells in the grid.
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

        // Generate walls around the grid.
        private void GenerateWall()
        {
            for (var i = _gameManager.gridMin.y; i < _gameManager.gridMax.y; i++) // Right
                GridManager.AddToGrids(new Vector2Int(_gameManager.gridMax.x - 1, i), "Wall");

            for (var i = _gameManager.gridMin.y; i < _gameManager.gridMax.y; i++) // Left
                GridManager.AddToGrids(new Vector2Int(_gameManager.gridMin.x, i), "Wall");

            for (var i = _gameManager.gridMin.y + 1; i < _gameManager.gridMax.x - 1; i++) // Top
                GridManager.AddToGrids(new Vector2Int(i, _gameManager.gridMax.y - 1), "Wall");

            for (var i = _gameManager.gridMin.y + 1; i < _gameManager.gridMax.x - 1; i++) // Bottom
                GridManager.AddToGrids(new Vector2Int(i, _gameManager.gridMin.y), "Wall");
        }

        // Generate the specified element on the grid.
        private void GenerateElement(string elem, int count, ICollection<string> occupiedTags, bool genAround = false, string aroundElem = "")
        {
            _agentComponents = new Agent.Agent[count];
            for (var i = 0; i < count; i++)
            {
                Vector2Int coords;
                bool isValid;

                do
                {
                    coords = new Vector2Int(Random.Range(_gameManager.gridMin.x + 1, _gameManager.gridMax.x - 1),
                        Random.Range(_gameManager.gridMin.y + 1, _gameManager.gridMax.y - 1));

                    isValid = !_gameManager.Map[coords.x, coords.y].Exists(x => occupiedTags.Contains(x.tag));

                    // For Pit and Wumpus, check they're not adjacent to any Human start positions
                    if (isValid && (elem == "Pit" || elem == "Wumpus"))
                    {
                        var adjacentCoords = new[]
                        {
                            new Vector2Int(coords.x + 1, coords.y),
                            new Vector2Int(coords.x - 1, coords.y),
                            new Vector2Int(coords.x, coords.y + 1),
                            new Vector2Int(coords.x, coords.y - 1)
                        };

                        foreach (var adjCoord in adjacentCoords)
                        {
                            if (_gameManager.Map[adjCoord.x, adjCoord.y].Exists(x => x.CompareTag("StartCell")))
                            {
                                isValid = false;
                                break;
                            }
                        }
                    }
                } while (!isValid);

                // Instantiate and initialize human agents on the grid.
                if (elem == "Human")
                {
                    if (Instantiate(Resources.Load("Human"), transform) is not GameObject agent) continue;
                    _agentComponents[i] = agent.GetComponent<Agent.Agent>();
                    _agentComponents[i].Init(i, coords, _gameManager.nbWumpus);

                    GridManager.AddToGrids(coords, "StartCell");
                    _gameManager.agents.Add(agent);
                }

                // Add the element to the grid.
                GridManager.AddToGrids(coords, elem);

                // Generate elements around the main element (e.g., Stench around Wumpus, Breeze around Pit).
                if (genAround)
                    GenerateAroundCell(coords, aroundElem);
            }
        }

        // Generate elements around a specific cell.
        private static void GenerateAroundCell(Vector2Int coords, string element)
        {
            Generate(new Vector2Int(coords.x + 1, coords.y), element); // Right cell
            Generate(new Vector2Int(coords.x - 1, coords.y), element); // Left cell
            Generate(new Vector2Int(coords.x, coords.y + 1), element); // Top cell
            Generate(new Vector2Int(coords.x, coords.y - 1), element); // Bottom cell

            // Generate the specified element at the new coordinates.
            void Generate(Vector2Int newCoords, string elem)
            {
                if (!GameManager.Instance.Map[newCoords.x, newCoords.y].Exists(x => x.CompareTag(elem)) &&
                    !GameManager.Instance.Map[newCoords.x, newCoords.y].Exists(x => x.tag is "Wall"))
                    GridManager.AddToGrids(newCoords, elem);
            }
        }
    }
}