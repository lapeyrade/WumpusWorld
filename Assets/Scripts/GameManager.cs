using System.Collections.Generic;
using System.Linq;
using UnityEngine;

public class GameManager : MonoBehaviour
{
    public static GameManager Instance;
    public bool isGameOver;
    public bool isModeAuto;

    public int randomSeed = 1;
    public Vector2Int gridMin = new(0, 0); 
    public Vector2Int gridMax = new(15, 15);
    public float tileSize = 1.05f;

    public List<GameObject>[,] Map;
    public List<GameObject>[,] AgentsMap;
    public List<GameObject> agents;
    
    // Initialise a Dictionary of <string, Color>
    private readonly Dictionary<string, Color> _cellColor = new()
    {
            {"start", Color.gray},
            {"wall", Color.black},
            {"visited", Color.cyan},
            {"danger", Color.red},
            {"safe", new Color(0.145f, 0.701f, 0.294f, 1)},
            {"undefined", new Color(1.0f, 0.64f, 0.0f)},
            {"cell", Color.white},
    };
    
    protected void Awake()
    {
        Instance = this;
        Random.InitState(randomSeed);

        Map = new List<GameObject>[gridMax.x, gridMax.y];
        AgentsMap = new List<GameObject>[gridMax.x, gridMax.y];
        agents = new List<GameObject>();
        
        GetComponent<GameBuilder>().BuildMaps();
    }
    
    public void AddToGrids(Vector2Int coords, string element)
    {
        if (element is "human") return;
        
        if (_cellColor.Keys.Contains(element))
            ChangeColorMap(coords, element);

        if (element is not "cell" && _cellColor.Keys.Contains(element)) return;
        AddGameObjectMap(AgentsMap, coords, element, GetAgentMapOffset(coords));
        AddGameObjectMap(Map, coords, element, GetWorldMapOffset(coords));
    }

    private void ChangeColorMap(Vector2Int coords, string element)
    {
        ChangeColorCell(AgentsMap, coords, element);
        if (element is "wall" or "start")
            ChangeColorCell(Map, coords, element);
    }

    private void ChangeColorCell(List<GameObject>[,] map, Vector2Int coords, string element)
    {
        if (map[coords.x, coords.y].Find(x => x.name is "cell") is not {} cell) return;
        if (map[coords.x, coords.y].Exists(x => x.CompareTag(element))) return;
        if (element == "wall" && map == AgentsMap && !Map[coords.x, coords.y].Exists( x => x.CompareTag(element))) return;
        if (element != "wall" && map[coords.x, coords.y].Find(x => x.name is "cell").tag is "start" or "wall" or "visited") return;
        if (element != "safe" && map[coords.x, coords.y].Find(x => x.name is "cell").tag is "danger") return;
        
        cell.tag = element;
        cell.GetComponent<SpriteRenderer>().color = _cellColor[element];
    }

    private void AddGameObjectMap(List<GameObject>[,] map, Vector2Int coords, string element, Vector2 newPosition)
    {
        if (map[coords.x, coords.y].Exists(x => x.name == element || x.tag is "wall")) return;
        if (element != "cell" && element != "wumpusdead" && map == AgentsMap &&
           !Map[coords.x, coords.y].Exists( x => x.name == element)) return;
        if (Instantiate(Resources.Load(element), transform) is not GameObject cell) return;
        cell.tag = element;
        cell.name = element;
        cell.transform.position = newPosition;
        map[coords.x, coords.y].Add(cell);
    }

    public void RemoveFromGrids(Vector2Int coords, string element)
    {
        RemoveGameObjectMap(AgentsMap, coords, element);
        RemoveGameObjectMap(Map, coords, element);
    }
    
    private void RemoveGameObjectMap(List<GameObject>[,] map, Vector2Int coords, string element)
    {
        if (!map[coords.x, coords.y].Exists(x => x.name == element)) return;
        GameObject cellMap = map[coords.x, coords.y].Find(x => x.CompareTag(element));
        if (cellMap.name is "cell") return;
        Destroy(cellMap);
        map[coords.x, coords.y].Remove(cellMap);
    }

    public bool CellInGridLimits(Vector2Int cell) => 
        cell.x >= gridMin.x && cell.x < gridMax.x && cell.y >= gridMin.y && cell.y < gridMax.y;

    public void AttachGoldToAgent(Agent agent)
    {
        agent.prefabGoldAgent = Instantiate(Resources.Load("gold")) as GameObject;
        if (agent.prefabGoldAgent is not null)
            agent.prefabGoldAgent.transform.position = GetAgentMapOffset(agent.coords);
        agent.prefabGoldMap = Instantiate(Resources.Load("gold")) as GameObject;
        if (agent.prefabGoldMap is not null)
            agent.prefabGoldMap.transform.position = GetWorldMapOffset(agent.coords);
    }

    public Vector2 GetAgentMapOffset(Vector2Int coord) =>
        new((coord.x - gridMax.x / 1.95f) * tileSize, coord.y * tileSize);

    public Vector2 GetWorldMapOffset(Vector2Int coord) =>
        new((coord.x + gridMax.x / 1.95f) * tileSize, coord.y * tileSize);
    
    public void SetGameOver(string message, bool exitApp)
    {
        isGameOver = true;
        Debug.Log(message);

        if (!exitApp) return;

        Application.Quit();
        UnityEditor.EditorApplication.isPlaying = false;
    }
}