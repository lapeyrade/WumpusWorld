using System;
using System.Collections.Generic;
using System.Linq;
using Prolog;
using UnityEngine;

namespace GameManagement
{
    public class GridManager : MonoBehaviour
    {
        private static GameManager _gameManager;
        private static PrologInterface _prologInterface;

        // Define the cell colors for visual representation
        private static readonly Dictionary<string, Color> CellColor = new()
        {
            {"StartCell", Color.gray},      // Starting position
            {"Wall", Color.black},          // Boundary walls
            {"VisitedCell", Color.cyan},    // Cells the agent has explored
            {"DangerousCell", Color.red},   // Cells containing hazards
            {"SafeCell", new Color(0.145f, 0.701f, 0.294f, 1)},  // Confirmed safe cells
            {"UnknownCell", new Color(1.0f, 0.64f, 0.0f)},       // Unexplored cells
            {"Cell", Color.white},          // Default cell color
        };

        private void Awake()
        {
            _gameManager = GameManager.Instance;
            // Initialize Prolog interface if using Prolog AI
            if (_gameManager.aiType is GameManager.AIType.Prolog)
                _prologInterface = _gameManager.GetComponent<PrologInterface>();
        }

        // Add an element to both the world map and agent's knowledge map
        public static void AddToGrids(Vector2Int coords, string element)
        {
            // Skip adding Human elements as they're handled separately
            if (element == "Human") return;

            // Update cell color if the element has a defined color
            if (CellColor.Keys.Contains(element))
                ChangeColorMap(coords, element);

            // Skip adding game objects for colored cells (except default cells)
            if (element != "Cell" && CellColor.Keys.Contains(element)) return;
            
            // Add the element to both maps with appropriate offsets
            AddGameObjectMap(_gameManager.AgentsMap, coords, element, GetAgentMapOffset(coords));
            AddGameObjectMap(_gameManager.Map, coords, element, GetWorldMapOffset(coords));
        }

        // Update cell colors in both maps
        private static void ChangeColorMap(Vector2Int coords, string element)
        {
            ChangeColorCell(_gameManager.AgentsMap, coords, element);
            // Only update world map for permanent elements
            if (element == "Wall" || element == "StartCell")
                ChangeColorCell(_gameManager.Map, coords, element);
        }

        // Change the color of a specific cell
        private static void ChangeColorCell(List<GameObject>[,] map, Vector2Int coords, string element)
        {
            var cell = map[coords.x, coords.y].Find(x => x.name == "Cell");
            if (cell == null) return;

            // Skip if cell already has this element
            if (map[coords.x, coords.y].Exists(x => x.CompareTag(element))) return;

            // Special handling for walls in agent map
            if (element == "Wall" && map == _gameManager.AgentsMap &&
                !_gameManager.Map[coords.x, coords.y].Exists(x => x.CompareTag(element))) return;

            // Preserve certain cell states
            if (element != "Wall" &&
                map[coords.x, coords.y].Find(x => x.name == "Cell").tag is "StartCell" or "Wall" or "VisitedCell") return;
            if (element != "SafeCell" && map[coords.x, coords.y].Find(x => x.name == "Cell").tag is "DangerousCell") return;

            cell.tag = element;

            // Update Prolog knowledge base if using Prolog AI
            if (_gameManager.aiType is GameManager.AIType.Prolog && map == _gameManager.AgentsMap)
                _prologInterface.QueryText +=
                    $", assertz(data_concept([{element.ToLower()}, [{coords.x}, {coords.y}]], {element.ToLower()})), " +
                    $"assertz({element.ToLower()}([{element.ToLower()}, [{coords.x}, {coords.y}]]))";

            cell.GetComponent<SpriteRenderer>().color = CellColor[element];
        }

        // Add a game object to a specific map
        private static void AddGameObjectMap(List<GameObject>[,] map, Vector2Int coords, string element, Vector2 newPosition)
        {
            // Skip if element already exists or cell is blocked by wall
            if (map[coords.x, coords.y].Exists(x => x.name == element || x.tag is "Wall")) return;
            
            // Verify element exists in world map before adding to agent map
            if (element != "Cell" && element != "DeadWumpus" && map == _gameManager.AgentsMap &&
               !_gameManager.Map[coords.x, coords.y].Exists(x => x.name == element)) return;

            // Create and configure new game object
            if (Instantiate(Resources.Load(element)) is not GameObject cell) return;
            cell.tag = element;

            // Update Prolog knowledge base if using Prolog AI
            if (_gameManager.aiType is GameManager.AIType.Prolog && map == _gameManager.AgentsMap)
                _prologInterface.QueryText +=
                    $", assertz(data_concept([{element.ToLower()}, [{coords.x}, {coords.y}]], {element.ToLower()})), " +
                    $"assertz({element.ToLower()}([{element.ToLower()}, [{coords.x}, {coords.y}]]))";

            cell.name = element;
            cell.transform.position = newPosition;
            cell.AddComponent(Type.GetType("Ontology." + element[0].ToString().ToUpper() + element[1..]));
            map[coords.x, coords.y].Add(cell);
        }

        // Remove an element from both maps
        public static void RemoveFromGrids(Vector2Int coords, string element)
        {
            RemoveGameObjectMap(_gameManager.AgentsMap, coords, element);
            RemoveGameObjectMap(_gameManager.Map, coords, element);
        }

        // Remove a game object from a specific map
        private static void RemoveGameObjectMap(List<GameObject>[,] map, Vector2Int coords, string element)
        {
            if (!map[coords.x, coords.y].Exists(x => x.name == element)) return;
            var cellMap = map[coords.x, coords.y].Find(x => x.CompareTag(element));
            if (cellMap.name == "Cell") return;
            
            // Deactivate object instead of destroying for potential reuse
            cellMap.SetActive(false);
            map[coords.x, coords.y].Remove(cellMap);

            // Update Prolog knowledge base if using Prolog AI
            if (_gameManager.aiType is GameManager.AIType.Prolog && map == _gameManager.AgentsMap)
                _prologInterface.QueryText +=
                    $", retract(data_concept([{element.ToLower()}, [{coords.x}, {coords.y}]], {element.ToLower()})), " +
                    $"retract({element.ToLower()}([{element.ToLower()}, [{coords.x}, {coords.y}]]))";
        }

        // Check if coordinates are within grid boundaries
        public static bool CellInGridLimits(Vector2Int cell) =>
            cell.x >= _gameManager.gridMin.x && cell.x < _gameManager.gridMax.x &&
            cell.y >= _gameManager.gridMin.y && cell.y < _gameManager.gridMax.y;

        // Attach gold visualization to an agent
        public static void AttachGoldToAgent(Agent.Agent agent)
        {
            // Create gold visualization for agent map
            agent.prefabGoldAgent = Instantiate(Resources.Load("gold")) as GameObject;
            if (agent.prefabGoldAgent != null)
                agent.prefabGoldAgent.transform.position = GetAgentMapOffset(agent.coords);
            
            // Create gold visualization for world map
            agent.prefabGoldMap = Instantiate(Resources.Load("gold")) as GameObject;
            if (agent.prefabGoldMap != null)
                agent.prefabGoldMap.transform.position = GetWorldMapOffset(agent.coords);
        }

        // Calculate position offset for agent map display
        public static Vector2 GetAgentMapOffset(Vector2Int coords) =>
            new((coords.x - _gameManager.gridMax.x / 1.95f) * _gameManager.tileSize, coords.y * _gameManager.tileSize);

        // Calculate position offset for world map display
        public static Vector2 GetWorldMapOffset(Vector2Int coords) =>
            new((coords.x + _gameManager.gridMax.x / 1.95f) * _gameManager.tileSize, coords.y * _gameManager.tileSize);
    }
}