using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using Ontology;
using UnityEngine;

public class GridManager : MonoBehaviour
{
    private static readonly Dictionary<string, Color> CellColor = new()
    {
        {"start", Color.gray},
        {"wall", Color.black},
        {"visited", Color.cyan},
        {"danger", Color.red},
        {"safe", new Color(0.145f, 0.701f, 0.294f, 1)},
        {"undefined", new Color(1.0f, 0.64f, 0.0f)},
        {"cell", Color.white},
    };
    
    public static void AddToGrids(Vector2Int coords, string element)
    {
        if (element is "human") return;
        
        if (CellColor.Keys.Contains(element))
            ChangeColorMap(coords, element);

        if (element is not "cell" && CellColor.Keys.Contains(element)) return;
        AddGameObjectMap(GameManager.Instance.AgentsMap, coords, element, GetAgentMapOffset(coords));
        AddGameObjectMap(GameManager.Instance.Map, coords, element, GetWorldMapOffset(coords));
    }

    private static void ChangeColorMap(Vector2Int coords, string element)
    {
        ChangeColorCell(GameManager.Instance.AgentsMap, coords, element);
        if (element is "wall" or "start")
            ChangeColorCell(GameManager.Instance.Map, coords, element);
    }

    private static void ChangeColorCell(List<GameObject>[,] map, Vector2Int coords, string element)
    {
        if (map[coords.x, coords.y].Find(x => x.name is "cell") is not {} cell) return;
        if (map[coords.x, coords.y].Exists(x => x.CompareTag(element))) return;
        if (element == "wall" && map == GameManager.Instance.AgentsMap &&
            !GameManager.Instance.Map[coords.x, coords.y].Exists( x => x.CompareTag(element))) return;
        if (element != "wall" &&
            map[coords.x, coords.y].Find(x => x.name is "cell").tag is "start" or "wall" or "visited") return;
        if (element != "safe" && map[coords.x, coords.y].Find(x => x.name is "cell").tag is "danger") return;
        
        cell.tag = element;
        cell.GetComponent<SpriteRenderer>().color = CellColor[element];
    }

    private static void AddGameObjectMap(List<GameObject>[,] map, Vector2Int coords, string element, Vector2 newPosition)
    {
        if (map[coords.x, coords.y].Exists(x => x.name == element || x.tag is "wall")) return;
        if (element != "cell" && element != "wumpusdead" && map == GameManager.Instance.AgentsMap &&
           !GameManager.Instance.Map[coords.x, coords.y].Exists( x => x.name == element)) return;
        if (Instantiate(Resources.Load(element)) is not GameObject cell) return;
        cell.tag = element;
        cell.name = element;
        cell.transform.position = newPosition;
        map[coords.x, coords.y].Add(cell);
    }

    public static void RemoveFromGrids(Vector2Int coords, string element)
    {
        RemoveGameObjectMap(GameManager.Instance.AgentsMap, coords, element);
        RemoveGameObjectMap(GameManager.Instance.Map, coords, element);
    }
    
    private static void RemoveGameObjectMap(List<GameObject>[,] map, Vector2Int coords, string element)
    {
        if (!map[coords.x, coords.y].Exists(x => x.name == element)) return;
        var cellMap = map[coords.x, coords.y].Find(x => x.CompareTag(element));
        if (cellMap.name is "cell") return;
        Destroy(cellMap);
        map[coords.x, coords.y].Remove(cellMap);
    }

    public static bool CellInGridLimits(Vector2Int cell) => 
        cell.x >= GameManager.Instance.gridMin.x && cell.x < GameManager.Instance.gridMax.x &&
        cell.y >= GameManager.Instance.gridMin.y && cell.y < GameManager.Instance.gridMax.y;

    public static void AttachGoldToAgent(Agent.Agent agent)
    {
        agent.prefabGoldAgent = Instantiate(Resources.Load("gold")) as GameObject;
        if (agent.prefabGoldAgent is not null)
            agent.prefabGoldAgent.transform.position = GetAgentMapOffset(agent.coords);
        agent.prefabGoldMap = Instantiate(Resources.Load("gold")) as GameObject;
        if (agent.prefabGoldMap is not null)
            agent.prefabGoldMap.transform.position = GetWorldMapOffset(agent.coords);
    }

    public static Vector2 GetAgentMapOffset(Vector2Int coord) =>
        new((coord.x - GameManager.Instance.gridMax.x / 1.95f)
         * GameManager.Instance.tileSize, coord.y * GameManager.Instance.tileSize);

    public static Vector2 GetWorldMapOffset(Vector2Int coord) =>
        new((coord.x + GameManager.Instance.gridMax.x / 1.95f)
         * GameManager.Instance.tileSize, coord.y * GameManager.Instance.tileSize);
}
