using System;
using System.Collections.Generic;
using System.Linq;
using Ontology;
using Prolog;
using UnityEngine;

namespace GameManagement
{
    public class GridManager : MonoBehaviour
    {
        private static GameManager _gameManager;
        private static PrologInterface _prologInterface;

        // Define the cell colors
        private static readonly Dictionary<string, Color> CellColor = new()
        {
            {"StartCell", Color.gray},
            {"Wall", Color.black},
            {"VisitedCell", Color.cyan},
            {"DangerousCell", Color.red},
            {"SafeCell", new Color(0.145f, 0.701f, 0.294f, 1)},
            {"UnknownCell", new Color(1.0f, 0.64f, 0.0f)},
            {"Cell", Color.white},
        };

        private void Awake()
        {
            _gameManager = GameManager.Instance;
            if (_gameManager.aiType is GameManager.AIType.Prolog)
                _prologInterface = _gameManager.GetComponent<PrologInterface>();
        }

        // Add an element to the grids
        public static void AddToGrids(Vector2Int coords, string element)
        {
            if (element == "Human") return;

            if (CellColor.Keys.Contains(element))
                ChangeColorMap(coords, element);

            if (element != "Cell" && CellColor.Keys.Contains(element)) return;
            AddGameObjectMap(_gameManager.AgentsMap, coords, element, GetAgentMapOffset(coords));
            AddGameObjectMap(_gameManager.Map, coords, element, GetWorldMapOffset(coords));
        }

        // Change the color of a cell in the map
        private static void ChangeColorMap(Vector2Int coords, string element)
        {
            ChangeColorCell(_gameManager.AgentsMap, coords, element);
            if (element == "Wall" || element == "StartCell")
                ChangeColorCell(_gameManager.Map, coords, element);
        }

        // Change the color of a cell in a specific map
        private static void ChangeColorCell(List<GameObject>[,] map, Vector2Int coords, string element)
        {
            var cell = map[coords.x, coords.y].Find(x => x.name == "Cell");
            if (cell == null) return;
            if (map[coords.x, coords.y].Exists(x => x.CompareTag(element))) return;
            if (element == "Wall" && map == _gameManager.AgentsMap &&
                !_gameManager.Map[coords.x, coords.y].Exists(x => x.CompareTag(element))) return;
            if (element != "Wall" &&
                map[coords.x, coords.y].Find(x => x.name == "Cell").tag is "StartCell" or "Wall" or "VisitedCell") return;
            if (element != "SafeCell" && map[coords.x, coords.y].Find(x => x.name == "Cell").tag is "DangerousCell") return;

            cell.tag = element;

            if (_gameManager.aiType is GameManager.AIType.Prolog && map == _gameManager.AgentsMap)
                _prologInterface.QueryText +=
                    $", assertz(data_concept([{element.ToLower()}, [{coords.x}, {coords.y}]], {element.ToLower()})), " +
                    $"assertz({element.ToLower()}([{element.ToLower()}, [{coords.x}, {coords.y}]]))";

            cell.GetComponent<SpriteRenderer>().color = CellColor[element];

            var cellComponents = cell.GetComponents<Component>().Where(x => x is Cell).ToArray();
            foreach (var component in cellComponents)
                Destroy(component);

            cell.AddComponent(Type.GetType("Ontology." + element[0].ToString().ToUpper() + element[1..]));
        }

        // Add a game object to a map
        private static void AddGameObjectMap(List<GameObject>[,] map, Vector2Int coords, string element, Vector2 newPosition)
        {
            if (map[coords.x, coords.y].Exists(x => x.name == element || x.tag is "Wall")) return;
            if (element != "Cell" && element != "DeadWumpus" && map == _gameManager.AgentsMap &&
               !_gameManager.Map[coords.x, coords.y].Exists(x => x.name == element)) return;
            if (Instantiate(Resources.Load(element)) is not GameObject cell) return;
            cell.tag = element;

            if (_gameManager.aiType is GameManager.AIType.Prolog && map == _gameManager.AgentsMap)
                _prologInterface.QueryText +=
                    $", assertz(data_concept([{element.ToLower()}, [{coords.x}, {coords.y}]], {element.ToLower()})), " +
                    $"assertz({element.ToLower()}([{element.ToLower()}, [{coords.x}, {coords.y}]]))";

            cell.name = element;
            cell.transform.position = newPosition;
            cell.AddComponent(Type.GetType("Ontology." + element[0].ToString().ToUpper() + element[1..]));
            map[coords.x, coords.y].Add(cell);
        }

        // Remove an element from the grids
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
            Destroy(cellMap);
            map[coords.x, coords.y].Remove(cellMap);

            if (_gameManager.aiType is GameManager.AIType.Prolog && map == _gameManager.AgentsMap)
                _prologInterface.QueryText +=
                    $", retract(data_concept([{element.ToLower()}, [{coords.x}, {coords.y}]], {element.ToLower()})), " +
                    $"retract({element.ToLower()}([{element.ToLower()}, [{coords.x}, {coords.y}]]))";
        }

        // Check if a cell is within grid limits
        public static bool CellInGridLimits(Vector2Int cell) =>
            cell.x >= _gameManager.gridMin.x && cell.x < _gameManager.gridMax.x &&
            cell.y >= _gameManager.gridMin.y && cell.y < _gameManager.gridMax.y;

        // Attach gold to an agent
        public static void AttachGoldToAgent(Agent.Agent agent)
        {
            agent.prefabGoldAgent = Instantiate(Resources.Load("gold")) as GameObject;
            if (agent.prefabGoldAgent != null)
                agent.prefabGoldAgent.transform.position = GetAgentMapOffset(agent.coords);
            agent.prefabGoldMap = Instantiate(Resources.Load("gold")) as GameObject;
            if (agent.prefabGoldMap != null)
                agent.prefabGoldMap.transform.position = GetWorldMapOffset(agent.coords);
        }

        // Get the agent map offset for a given set of coordinates
        public static Vector2 GetAgentMapOffset(Vector2Int coords) =>
            new((coords.x - _gameManager.gridMax.x / 1.95f) * _gameManager.tileSize, coords.y * _gameManager.tileSize);

        // Get the world map offset for a given set of coordinates
        public static Vector2 GetWorldMapOffset(Vector2Int coords) =>
            new((coords.x + _gameManager.gridMax.x / 1.95f) * _gameManager.tileSize, coords.y * _gameManager.tileSize);
    }
}