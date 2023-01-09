using System.Collections.Generic;
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

    public List<(string, GameObject)>[,] Map;
    public List<(string, GameObject)>[,] AgentsMap;

    public List<Human> agents;

    protected void Awake()
    {
        Instance = this;
        Random.InitState(randomSeed);

        Map = new List<(string, GameObject)>[gridMax.x, gridMax.y];
        AgentsMap = new List<(string, GameObject)>[gridMax.x, gridMax.y];

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
        for (int x = gridMin.x; x < gridMax.x; x++)
        {
            for (int y = gridMin.y; y < gridMax.y; y++)
            {
                AgentsMap[x, y] = new List<(string, GameObject)>();
                Map[x, y] = new List<(string, GameObject)>();
                AddToGrids(new Vector2Int(x, y), "cell", true, true);
            }
        }
    }

    private void GenerateWall()
    {
        for (int y = gridMin.y; y < gridMax.y; y++) // Right
            AddToGrids(new Vector2Int(gridMax.x - 1, y), "wall", false, true);

        for (int y = gridMin.y; y < gridMax.y; y++) // Left
            AddToGrids(new Vector2Int(gridMin.x, y), "wall", false, true);

        for (int x = gridMin.y; x < gridMax.x; x++) // Top
            AddToGrids(new Vector2Int(x, gridMax.y - 1), "wall", false, true);

        for (int x = gridMin.y; x < gridMax.x; x++) // Bottom
            AddToGrids(new Vector2Int(x, gridMin.y), "wall", false, true);
    }

    private void GenerateHuman()
    {
        agents = new List<Human>();

        for (int i = 0; i < nbAgent; i++)
        {
            Vector2Int coord;

            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.x - 1), Random.Range(gridMin.y + 1, gridMax.y - 1));
            } while (Map[coord.x, coord.y].Exists(x => x.Item1 is "start" or "wall"));

            Human agent = new(i, "human", coord, nbWumpus)
            {
                prefabAgentMap = Instantiate(Resources.Load("human"), transform) as GameObject,
                prefabWorldMap = Instantiate(Resources.Load("human"), transform) as GameObject
            };

            if (agent.prefabAgentMap != null) 
                agent.prefabAgentMap.transform.position = GetAgentMapOffset(coord);
            if (agent.prefabWorldMap != null) 
                agent.prefabWorldMap.transform.position = GetWorldMapOffset(coord);

            AddToGrids(coord, "start", false, true);
            
            agents.Add(agent);
        }
    }

    private void GenerateGold()
    {
        for (int gold = 0; gold < nbGold; gold++)
        {
            Vector2Int coord;
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.x - 1), Random.Range(gridMin.y + 1, gridMax.y - 1));
            } while (Map[coord.x, coord.y].Exists(x => x.Item1 is "start" or "wall" or "gold"));

            AddToGrids(coord, "gold", false, true);
        }
    }

    private void GenerateWumpus()
    {
        for (int wumpus = 0; wumpus < nbWumpus; wumpus++)
        {
            Vector2Int coord;
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.x - 1), Random.Range(gridMin.y + 1, gridMax.y - 1));
            } while (Map[coord.x, coord.y].Exists(x => x.Item1 is "start" or "wall" or "gold" or "wumpus"));

            AddToGrids(coord, "wumpus", false, true);
            GenerateAroundCell(coord, "stench");
        }
    }

    private void GeneratePit()
    {
        for (int pit = 0; pit < nbPit; pit++)
        {
            Vector2Int coord;
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.x - 1), Random.Range(gridMin.y + 1, gridMax.y - 1));
            } while (Map[coord.x, coord.y].Exists(x => x.Item1 is "start" or "wall" or "gold" or "wumpus" or "pit"));

            AddToGrids(coord, "pit", false, true);
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
            if (!Map[coords.x, coords.y].Exists(x => x.Item1 == elem) &&
                !Map[coords.x, coords.y].Exists(x => x.Item1 == "wall"))
                AddToGrids(coords, elem, false, true);
        }
    }

    public void AddToGrids(Vector2Int coord, string content, bool updateAgentsMap, bool updatedMap)
    {
        if (content is "wall" or "safe" or "visited" or "danger" or "undefined" or "start")
        {
            Color newColor = content switch
            {
                "start" => Color.gray,
                "wall" => Color.black,
                "visited" => Color.cyan,
                "danger" => Color.red,
                "safe" => new Color(0.145f, 0.701f, 0.294f, 1),
                "undefined" => new Color(1.0f, 0.64f, 0.0f),
                _ => Color.white
            };

            if (content is "visited" or "safe" or "undefined"
                && AgentsMap[coord.x, coord.y].Exists(x => x.Item1 == "start"))
                return;

            if (updateAgentsMap && !AgentsMap[coord.x, coord.y].Exists(x =>
                    x.Item1 == content || (x.Item1 == "visited" && content != "wall" && content != "danger")))
                ChangeColorMap(AgentsMap, coord, content, newColor);

            if (updatedMap && !Map[coord.x, coord.y].Exists(x => x.Item1 == content))
                ChangeColorMap(Map, coord, content, newColor);
        }
        else
        {
            if (updateAgentsMap && content != "human")
                AddGameObjectMap(AgentsMap, coord, content, GetAgentMapOffset(coord));

            if (updatedMap && content != "human")
                AddGameObjectMap(Map, coord, content, GetWorldMapOffset(coord));
        }

        void ChangeColorMap(List<(string, GameObject)>[,] map, Vector2Int coords, string element, Color newColor)
        {
            map[coords.x, coords.y].Add((element, null));
            map[coords.x, coords.y].Find(x => x.Item1 == "cell").Item2.GetComponent<SpriteRenderer>().color = newColor;
        }

        void AddGameObjectMap(List<(string, GameObject)>[,] map, Vector2Int coords, string element, Vector2 newPosition)
        {
            if (map[coords.x, coords.y].Exists(x => x.Item1 == element)) return;
            
            map[coords.x, coords.y].Add((element, Instantiate(Resources.Load(element), transform) as GameObject));
            map[coords.x, coords.y].Find(x => x.Item1 == element).Item2.transform.position = newPosition;
        }
    }

    public void RemoveFromGrids(Vector2Int coord, string element, bool updateAgentsMap, bool updateMap)
    {
        if (updateAgentsMap && AgentsMap[coord.x, coord.y].Exists(x => x.Item1 == element))
        {
            Destroy(AgentsMap[coord.x, coord.y].Find(x => x.Item1 == element).Item2);
            AgentsMap[coord.x, coord.y].Remove(AgentsMap[coord.x, coord.y].Find(x => x.Item1 == element));
        }

        if (!updateMap || !Map[coord.x, coord.y].Exists(x => x.Item1 == element)) return;
        Destroy(Map[coord.x, coord.y].Find(x => x.Item1 == element).Item2);
        Map[coord.x, coord.y].Remove(Map[coord.x, coord.y].Find(x => x.Item1 == element));
    }

    public void AttachGoldToAgent(Human agent)
    {
        agent.prefabGoldAgent = Instantiate(Resources.Load("gold")) as GameObject;
        if (agent.prefabGoldAgent != null)
            agent.prefabGoldAgent.transform.position = GetAgentMapOffset(agent.coord);
        agent.prefabGoldMap = Instantiate(Resources.Load("gold")) as GameObject;
        if (agent.prefabGoldMap != null)
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