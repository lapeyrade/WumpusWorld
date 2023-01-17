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
    public int nbPit = 4;
    public int nbWumpus = 2;
    public int nbGold = 1;
    public int nbAgent = 2;

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

        if ((nbPit + nbWumpus + nbGold + nbAgent) > (gridMax.x * gridMax.y))
        {
            Debug.LogError("Map too small, can't contain all the elements.");
            Application.Quit();
            UnityEditor.EditorApplication.isPlaying = false;
        }
    
        GenerateGrid();
        GenerateWall();
        GenerateHuman();
        GenerateGold();
        GenerateWumpus();
        GeneratePit();
        GameObject.Find("Main Camera").GetComponent<CameraController>().AdjustCameraPosition();
    }
    
    private void GenerateGrid()
    {
        for (int i = gridMin.x; i < gridMax.x; i++)
        {
            for (int j = gridMin.y; j < gridMax.y; j++)
            {
                AgentsMap[i, j] = new List<GameObject>();
                Map[i, j] = new List<GameObject>();
                AddToGrids(new Vector2Int(i, j), "cell");
            }
        }
    }

    private void GenerateWall()
    {
        for (int i = gridMin.y; i < gridMax.y; i++) // Right
            AddToGrids(new Vector2Int(gridMax.x - 1, i), "wall");

        for (int i = gridMin.y; i < gridMax.y; i++) // Left
            AddToGrids(new Vector2Int(gridMin.x, i), "wall");

        for (int i = gridMin.y + 1; i < gridMax.x - 1; i++) // Top
            AddToGrids(new Vector2Int(i, gridMax.y - 1), "wall");

        for (int i = gridMin.y + 1; i < gridMax.x - 1; i++) // Bottom
            AddToGrids(new Vector2Int(i, gridMin.y), "wall");
    }

    private void GenerateHuman()
    {
        agents = new List<GameObject>();

        for (int i = 0; i < nbAgent; i++)
        {
            Vector2Int coord;

            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.x - 1), Random.Range(gridMin.y + 1, gridMax.y - 1));
            } while (Map[coord.x, coord.y].Exists(x => x.tag is "start" or "wall"));
            
            if (Instantiate(Resources.Load("human"), transform) is not GameObject agent) continue;
            agent.GetComponent<Agent>().Init(i, coord, nbWumpus);

            AddToGrids(coord, "start");
            agents.Add(agent);
        }
    }

    private void GenerateGold()
    {
        for (int i = 0; i < nbGold; i++)
        {
            Vector2Int coord;
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.x - 1), Random.Range(gridMin.y + 1, gridMax.y - 1));
            } while (Map[coord.x, coord.y].Exists(x => x.tag is "start" or "wall" or "gold"));

            AddToGrids(coord, "gold");
        }
    }

    private void GenerateWumpus()
    {
        for (int i = 0; i < nbWumpus; i++)
        {
            Vector2Int coord;
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.x - 1), Random.Range(gridMin.y + 1, gridMax.y - 1));
            } while (Map[coord.x, coord.y].Exists(x => x.tag is "start" or "wall" or "gold" or "wumpus"));

            AddToGrids(coord, "wumpus");
            GenerateAroundCell(coord, "stench");
        }
    }

    private void GeneratePit()
    {
        for (int i = 0; i < nbPit; i++)
        {
            Vector2Int coord;
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.x - 1), Random.Range(gridMin.y + 1, gridMax.y - 1));
            } while (Map[coord.x, coord.y].Exists(x => x.tag is "start" or "wall" or "gold" or "wumpus" or "pit"));

            AddToGrids(coord, "pit");
            GenerateAroundCell(coord, "breeze");
        }
    }

    private void GenerateAroundCell(Vector2Int coord, string element)
    {
        Generate(new Vector2Int(coord.x + 1, coord.y), element); // Right cell
        Generate(new Vector2Int(coord.x - 1, coord.y), element); // Left cell
        Generate(new Vector2Int(coord.x, coord.y + 1), element); // Top cell
        Generate(new Vector2Int(coord.x, coord.y - 1), element); // Bottom cell

        void Generate(Vector2Int coords, string elem)
        {
            if (!Map[coords.x, coords.y].Exists(x => x.CompareTag(elem)) &&
                !Map[coords.x, coords.y].Exists(x => x.tag is "wall"))
                AddToGrids(coords, elem);
        }
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

    public void AttachGoldToAgent(Agent agent)
    {
        agent.prefabGoldAgent = Instantiate(Resources.Load("gold")) as GameObject;
        if (agent.prefabGoldAgent is not null)
            agent.prefabGoldAgent.transform.position = GetAgentMapOffset(agent.coord);
        agent.prefabGoldMap = Instantiate(Resources.Load("gold")) as GameObject;
        if (agent.prefabGoldMap is not null)
            agent.prefabGoldMap.transform.position = GetWorldMapOffset(agent.coord);
            
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