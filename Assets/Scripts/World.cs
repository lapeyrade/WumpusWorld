using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// Construct and update the world
/// Manage the graphical part
/// </summary>
public class World : MonoBehaviour
{
    [SerializeField] public int randomSeed = 29;
    [SerializeField] public Vector2Int gridMin = new(0, 0);
    [SerializeField] public Vector2Int gridMax = new(9, 9);
    [SerializeField] public float tileSize = 1.05f;
    [SerializeField] public int nbPit = 4;
    [SerializeField] public int nbWumpus = 2;
    [SerializeField] public int nbGold = 1;
    [SerializeField] public int nbAgent = 2;

    public List<(string, GameObject)>[,] Map;
    public List<(string, GameObject)>[,] AgentMap;

    public GameObject gridManager;
    public GameObject mainCamera;
    private GameController _gameController;
    private CameraController _cameraController;
    private PrologInterface _prologInterface;
    [SerializeField] public List<Human> agents;


    protected void Awake()
    {
        _gameController = gridManager.GetComponent<GameController>();
        _prologInterface = gridManager.GetComponent<PrologInterface>();
    }

    protected void Start()
    {
        Random.InitState(randomSeed); // Random Seed

        Map = new List<(string, GameObject)>[gridMax.x, gridMax.y];
        AgentMap = new List<(string, GameObject)>[gridMax.x, gridMax.y];


        if ((nbPit + nbWumpus + nbGold + nbAgent) > (gridMax.x * gridMax.y))
        {
            Debug.LogError("Map too small.");
            _gameController.SetGameOver("Incorrect Parameters", true);
        }
        else
        {
            _prologInterface.InitKnowledgeBase();

            GenerateGrid();
            GenerateWall();
            GenerateHuman();
            GenerateGold();
            GenerateWumpus();
            GeneratePit();

            _gameController.PlayTurn(true);
        }

        _cameraController = mainCamera.GetComponent<CameraController>();
        _cameraController.AdjustCameraPosition();
    }

    private void GenerateGrid()
    {
        for (int x = gridMin.x; x < gridMax.x; x++)
        {
            for (int y = gridMin.y; y < gridMax.y; y++)
            {
                AgentMap[x, y] = new List<(string, GameObject)>();
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
            Vector2Int coord = new(1, 1);

            if (i != 0)
            {
                do
                {
                    coord = new(Random.Range(gridMin.x + 1, gridMax.y - 1), Random.Range(gridMin.x + 1, gridMax.y - 1));
                } while (Map[coord.x, coord.y].Exists(x => x.Item1 is "human" or "wall"));
            }

            Human agent = new(i, "human", coord, nbWumpus)
            {
                agentMapPrefab = Instantiate(Resources.Load("human")) as GameObject,
                worldMapPrefab = Instantiate(Resources.Load("human")) as GameObject
            };

            agents.Add(agent);
            _gameController.PlayTurn(true);
        }

        _prologInterface.InitialiseAgents(agents);
    }

    private void GenerateGold()
    {
        for (int gold = 0; gold < nbGold; gold++)
        {
            Vector2Int coord;
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.y - 1), Random.Range(gridMin.x + 1, gridMax.y - 1));
            } while (Map[coord.x, coord.y].Exists(x => x.Item1 is "human" or "wall" or "gold"));

            AddToGrids(coord, "gold", true, true);
            AgentMap[coord.x, coord.y].Find(x => x.Item1 == "gold").Item2.SetActive(false);
        }
    }

    private void GenerateWumpus()
    {
        for (int wumpus = 0; wumpus < nbWumpus; wumpus++)
        {
            Vector2Int coord;
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.y - 1), Random.Range(gridMin.x + 1, gridMax.y - 1));
            } while (Map[coord.x, coord.y].Exists(x => x.Item1 is "human" or "wall" or "gold" or "wumpus"));

            AddToGrids(coord, "wumpus", true, true);
            AgentMap[coord.x, coord.y].Find(x => x.Item1 == "wumpus").Item2.SetActive(false);

            AddToGrids(coord, "wumpusdead", true, true);
            Map[coord.x, coord.y].Find(x => x.Item1 == "wumpusdead").Item2.SetActive(false);
            AgentMap[coord.x, coord.y].Find(x => x.Item1 == "wumpusdead").Item2.SetActive(false);

            GenerateAroundCell(coord, "stenchyes");
        }
    }

    private void GeneratePit()
    {
        for (int pit = 0; pit < nbPit; pit++)
        {
            Vector2Int coord;
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.y - 1), Random.Range(gridMin.x + 1, gridMax.y - 1));
            } while (Map[coord.x, coord.y].Exists(x => x.Item1 is "human" or "wall" or "gold" or "wumpus" or "pit"));

            AddToGrids(coord, "pit", true, true);
            AgentMap[coord.x, coord.y].Find(x => x.Item1 == "pit").Item2.SetActive(false);

            GenerateAroundCell(coord, "breezeyes");
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
            if (!Map[coords.x, coords.y].Exists(x => x.Item1 == elem) && !Map[coords.x, coords.y].Exists(x => x.Item1 == "wall"))
            {
                AddToGrids(coords, elem, true, true);
                AgentMap[coords.x, coords.y].Find(x => x.Item1 == elem).Item2.SetActive(false);
            }
        }
    }

    public void AddToGrids(Vector2Int coord, string content, bool updateAgentMap, bool updatedMap)
    {
        if (content is "wall" or "safe" or "visited" or "danger" or "undefined")
        {
            Color newColor = Color.white;

            switch (content)
            {
                case "wall": // Black 
                    newColor = Color.black;
                    break;
                case "visited": // Cyan
                    newColor = Color.cyan;
                    break;
                case "danger": // Red
                    newColor = Color.red;
                    break;
                case "safe": // Dark Green
                    newColor = new Color(0.145f, 0.701f, 0.294f, 1);
                    break;
                case "undefined": // Orange
                    newColor = new Color(1.0f, 0.64f, 0.0f);
                    break;
            }

            if (updateAgentMap && !AgentMap[coord.x, coord.y].Exists(x => x.Item1 == content || (x.Item1 == "visited" && content != "wall" && content != "danger")))
                ChangeColorMap(AgentMap, coord, content, newColor);

            if (updatedMap && !Map[coord.x, coord.y].Exists(x => x.Item1 == content))
                ChangeColorMap(Map, coord, content, newColor);
        }
        else
        {
            if (updateAgentMap && content != "human")
                AddGameObjectMap(AgentMap, coord, content, GetAgentMapOffset(coord));

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
            if (!map[coords.x, coords.y].Exists(x => x.Item1 == element))
            {
                map[coords.x, coords.y].Add((element, Instantiate(Resources.Load(element), transform) as GameObject));
                map[coords.x, coords.y].Find(x => x.Item1 == element).Item2.transform.position = newPosition;
            }
            else
                map[coords.x, coords.y].Find(x => x.Item1 == element).Item2.SetActive(true);
        }
    }

    public void RemoveFromGrids(Vector2Int coord, string content, bool updateAgentMap, bool updateMap)
    {
        if (updateAgentMap && AgentMap[coord.x, coord.y].Exists(x => x.Item1 == content))
        {
            if (AgentMap[coord.x, coord.y].Find(x => x.Item1 == content).Item2 != null)
                AgentMap[coord.x, coord.y].Find(x => x.Item1 == content).Item2.SetActive(false);
            AgentMap[coord.x, coord.y].Remove(AgentMap[coord.x, coord.y].Find(x => x.Item1 == content));
            _prologInterface.RemoveCellContentKb(coord, content);
        }

        if (updateMap && Map[coord.x, coord.y].Exists(x => x.Item1 == content))
        {
            if (Map[coord.x, coord.y].Find(x => x.Item1 == content).Item2 != null)
                Map[coord.x, coord.y].Find(x => x.Item1 == content).Item2.SetActive(false);
            Map[coord.x, coord.y].Remove(Map[coord.x, coord.y].Find(x => x.Item1 == content));
        }
    }

    public void ShootArrow(Human agent, string direction)
    {
        _prologInterface.RemoveFromKb("nb_arrow(_, _)");
        agent.nbArrow -= 1;
        _prologInterface.AddToKb($"nb_arrow({agent.agentName}, {agent.nbArrow})", true);

        switch (direction)
        {
            case "right":
                for (int x = agent.coord.x; x < gridMax.x; x++)
                {
                    if (Map[x, agent.coord.y].Exists(a => a.Item1 == "wumpus"))
                    {
                        KillWumpus(new Vector2Int(x, agent.coord.y));
                        break;
                    }
                }
                break;
            case "left":
                for (int x = agent.coord.x; x > gridMin.x; x--)
                {
                    if (Map[x, agent.coord.y].Exists(a => a.Item1 == "wumpus"))
                    {
                        KillWumpus(new Vector2Int(x, agent.coord.y));
                        break;
                    }
                }
                break;
            case "up":
                for (int y = agent.coord.y; y < gridMax.y; y++)
                {
                    if (Map[agent.coord.x, y].Exists(x => x.Item1 == "wumpus"))
                    {
                        KillWumpus(new Vector2Int(agent.coord.x, y));
                        break;
                    }
                }
                break;
            case "down":
                for (int y = agent.coord.y; y > gridMin.y; y--)
                {
                    if (Map[agent.coord.x, y].Exists(x => x.Item1 == "wumpus"))
                    {
                        KillWumpus(new Vector2Int(agent.coord.x, y));
                        break;
                    }
                }
                break;
        }

        void KillWumpus(Vector2Int coordWumpus)
        {
            RemoveFromGrids(coordWumpus, "wumpus", true, true);
            AddToGrids(coordWumpus, "wumpusdead", true, true);
            _prologInterface.AddCellContentKb(coordWumpus, "wumpusdead");
            _gameController.MakeInferences();
        }
    }

    public void Move(Human agent, Vector2Int newCoord)
    {
        RemoveFromGrids(agent.coord, agent.agentName, true, true);

        if (!Map[newCoord.x, newCoord.y].Exists(x => x.Item1 == agent.agentName))
        {
            Map[newCoord.x, newCoord.y].Add((agent.agentName, null));
            agent.worldMapPrefab.transform.position = GetAgentMapOffset(newCoord);
        }
        if (!AgentMap[newCoord.x, newCoord.y].Exists(x => x.Item1 == agent.agentName))
        {
            AgentMap[newCoord.x, newCoord.y].Add((agent.agentName, null));
            agent.agentMapPrefab.transform.position = GetAgentMapOffset(newCoord);
        }

        agent.Move(newCoord);

        _prologInterface.AddCellContentKb(agent.coord, agent.agentName);
        AddToGrids(agent.coord, "visited", true, false);
    }

    private Vector2 GetAgentMapOffset(Vector2Int coord) => 
        new ((coord.x - gridMax.x / 2 - 0.65f) * tileSize, coord.y * tileSize);
    
    private Vector2 GetWorldMapOffset(Vector2Int coord) =>
        new ((coord.x + gridMax.x / 2 + 0.65f) * tileSize, coord.y * tileSize);
}