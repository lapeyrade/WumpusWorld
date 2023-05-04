using System;
using System.Collections.Generic;
using System.Linq;
using Ontology;
using UnityEngine;

public class GridManager : MonoBehaviour
{
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
    
    public static void AddToGrids(Vector2Int coords, string element)
    {
        if (element is "Human") return;
        
        if (CellColor.Keys.Contains(element))
            ChangeColorMap(coords, element);

        if (element is not "Cell" && CellColor.Keys.Contains(element)) return;
        AddGameObjectMap(GameManager.Instance.AgentsMap, coords, element, GetAgentMapOffset(coords));
        AddGameObjectMap(GameManager.Instance.Map, coords, element, GetWorldMapOffset(coords));
    }

    private static void ChangeColorMap(Vector2Int coords, string element)
    {
        ChangeColorCell(GameManager.Instance.AgentsMap, coords, element);
        if (element is "Wall" or "StartCell")
            ChangeColorCell(GameManager.Instance.Map, coords, element);
    }

    private static void ChangeColorCell(List<GameObject>[,] map, Vector2Int coords, string element)
    {
        if (map[coords.x, coords.y].Find(x => x.name is "Cell") is not {} cell) return;
        if (map[coords.x, coords.y].Exists(x => x.CompareTag(element))) return;
        if (element == "Wall" && map == GameManager.Instance.AgentsMap &&
            !GameManager.Instance.Map[coords.x, coords.y].Exists( x => x.CompareTag(element))) return;
        if (element != "Wall" &&
            map[coords.x, coords.y].Find(x => x.name is "Cell").tag is "StartCell" or "Wall" or "VisitedCell") return;
        if (element != "SafeCell" && map[coords.x, coords.y].Find(x => x.name is "Cell").tag is "DangerousCell") return;
        
        cell.tag = element;
        cell.GetComponent<SpriteRenderer>().color = CellColor[element];
        
        foreach (var component in cell.GetComponents<Component>().Where(x => x is Cell))
            Destroy(component);
        
        cell.AddComponent(Type.GetType("Ontology." + element[0].ToString().ToUpper() + element[1..]));
    }

    private static void AddGameObjectMap(List<GameObject>[,] map, Vector2Int coords, string element, Vector2 newPosition)
    {
        if (map[coords.x, coords.y].Exists(x => x.name == element || x.tag is "Wall")) return;
        if (element != "Cell" && element != "DeadWumpus" && map == GameManager.Instance.AgentsMap &&
           !GameManager.Instance.Map[coords.x, coords.y].Exists( x => x.name == element)) return;
        if (Instantiate(Resources.Load(element)) is not GameObject cell) return;
        cell.tag = element;
        cell.name = element;
        cell.transform.position = newPosition;
        cell.AddComponent(Type.GetType("Ontology." + element[0].ToString().ToUpper() + element[1..]));
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
        if (cellMap.name is "Cell") return;
        Destroy(cellMap);
        map[coords.x, coords.y].Remove(cellMap);
    }

    public static bool CellInGridLimits(Vector2Int cell) => 
        cell.x >= GameManager.Instance.gridMin.x && cell.x < GameManager.Instance.gridMax.x &&
        cell.y >= GameManager.Instance.gridMin.y && cell.y < GameManager.Instance.gridMax.y;

    public static void AttachGoldToAgent(Agent.Agent agent)
    {
        agent.prefabGoldAgent = Instantiate(Resources.Load("Gold")) as GameObject;
        if (agent.prefabGoldAgent is not null)
            agent.prefabGoldAgent.transform.position = GetAgentMapOffset(agent.coords);
        agent.prefabGoldMap = Instantiate(Resources.Load("Gold")) as GameObject;
        if (agent.prefabGoldMap is not null)
            agent.prefabGoldMap.transform.position = GetWorldMapOffset(agent.coords);
    }

    public static Vector2 GetAgentMapOffset(Vector2Int coords) =>
        new((coords.x - GameManager.Instance.gridMax.x / 1.95f)
         * GameManager.Instance.tileSize, coords.y * GameManager.Instance.tileSize);

    public static Vector2 GetWorldMapOffset(Vector2Int coords) =>
        new((coords.x + GameManager.Instance.gridMax.x / 1.95f)
         * GameManager.Instance.tileSize, coords.y * GameManager.Instance.tileSize);
}
