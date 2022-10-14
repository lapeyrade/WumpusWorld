using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// Construct and update the world
/// Manage the graphical part
/// </summary>
public class World : MonoBehaviour
{
    [SerializeField] public int RandomSeed = 29;
    [SerializeField] public Vector2Int gridMin = new(0, 0);
    [SerializeField] public Vector2Int gridMax = new(9, 9);
    [SerializeField] public float tileSize = 1.05f;
    [SerializeField] public int nbPit = 4;
    [SerializeField] public int nbWumpus = 2;
    [SerializeField] public int nbGold = 1;
    [SerializeField] public int nbAgent = 2;

    public List<(string, GameObject)>[,] map;
    public List<(string, GameObject)>[,] agentMap;

    public GameObject gridManager;
    public GameObject mainCamera;
    private GameController gameController;
    private CameraController cameraController;
    private PrologInterface prologInterface;
    [SerializeField] public List<Human> agents;


    protected void Awake()
    {
        gameController = gridManager.GetComponent<GameController>();
        prologInterface = gridManager.GetComponent<PrologInterface>();
    }

    protected void Start()
    {
        Random.InitState(RandomSeed); // Random Seed

        map = new List<(string, GameObject)>[gridMax.x, gridMax.y];
        agentMap = new List<(string, GameObject)>[gridMax.x, gridMax.y];


        if ((nbPit + nbWumpus + nbGold + nbAgent) > (gridMax.x * gridMax.y))
        {
            Debug.LogError("Map too small.");
            gameController.SetGameOver("Incorrect Parameters", true);
        }
        else
        {
            prologInterface.InitKnowledgeBase();

            GenerateGrid();
            GrenerateWall();
            GenerateHuman();
            GenerateGold();
            GenerateWumpus();
            GeneratePit();

            gameController.PlayTurn(true);
        }

        cameraController = mainCamera.GetComponent<CameraController>();
        cameraController.AdjustCameraPosition();
    }

    private void GenerateGrid()
    {
        for (int x = gridMin.x; x < gridMax.x; x++)
        {
            for (int y = gridMin.y; y < gridMax.y; y++)
            {
                agentMap[x, y] = new List<(string, GameObject)>();
                map[x, y] = new List<(string, GameObject)>();
                AddToGrids(new Vector2Int(x, y), "cell", true, true);
            }
        }
    }

    private void GrenerateWall()
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
                } while (map[coord.x, coord.y].Exists(x => x.Item1 == "human" || x.Item1 == "wall"));
            }

            Human agent = new(i, "human", coord, nbWumpus)
            {
                agentMapPrefab = Instantiate(Resources.Load("human")) as GameObject,
                worldMapPrefab = Instantiate(Resources.Load("human")) as GameObject
            };

            agents.Add(agent);
            gameController.PlayTurn(true);
        }

        prologInterface.InitialiseAgents(agents);
    }

    private void GenerateGold()
    {
        Vector2Int coord = new(0, 0);

        for (int gold = 0; gold < nbGold; gold++)
        {
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.y - 1), Random.Range(gridMin.x + 1, gridMax.y - 1));
            } while (map[coord.x, coord.y].Exists(x => x.Item1 == "human" || x.Item1 == "wall" || x.Item1 == "gold"));

            AddToGrids(coord, "gold", true, true);
            agentMap[coord.x, coord.y].Find(x => x.Item1 == "gold").Item2.SetActive(false);
        }
    }

    private void GenerateWumpus()
    {
        Vector2Int coord = new(0, 0);

        for (int wumpus = 0; wumpus < nbWumpus; wumpus++)
        {
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.y - 1), Random.Range(gridMin.x + 1, gridMax.y - 1));
            } while (map[coord.x, coord.y].Exists(x => x.Item1 == "human" || x.Item1 == "wall" || x.Item1 == "gold" || x.Item1 == "wumpus"));

            AddToGrids(coord, "wumpus", true, true);
            agentMap[coord.x, coord.y].Find(x => x.Item1 == "wumpus").Item2.SetActive(false);

            AddToGrids(coord, "wumpusdead", true, true);
            map[coord.x, coord.y].Find(x => x.Item1 == "wumpusdead").Item2.SetActive(false);
            agentMap[coord.x, coord.y].Find(x => x.Item1 == "wumpusdead").Item2.SetActive(false);

            GenerateAroundCell(coord, "stenchyes");
        }
    }

    private void GeneratePit()
    {
        Vector2Int coord = new(0, 0);

        for (int pit = 0; pit < nbPit; pit++)
        {
            do
            {
                coord = new(Random.Range(gridMin.x + 1, gridMax.y - 1), Random.Range(gridMin.x + 1, gridMax.y - 1));
            } while (map[coord.x, coord.y].Exists(x => x.Item1 == "human" || x.Item1 == "wall" || x.Item1 == "gold" || x.Item1 == "wumpus" || x.Item1 == "pit"));

            AddToGrids(coord, "pit", true, true);
            agentMap[coord.x, coord.y].Find(x => x.Item1 == "pit").Item2.SetActive(false);

            GenerateAroundCell(coord, "breezeyes");
        }
    }

    private void GenerateAroundCell(Vector2Int coord, string element)
    {
        Generate(new Vector2Int(coord.x + 1, coord.y), element); // Right cell
        Generate(new Vector2Int(coord.x - 1, coord.y), element); // Left cell
        Generate(new Vector2Int(coord.x, coord.y + 1), element); // Top cell
        Generate(new Vector2Int(coord.x, coord.y - 1), element); // Bottom cell

        void Generate(Vector2Int coord, string element)
        {
            if (!map[coord.x, coord.y].Exists(x => x.Item1 == element) && !map[coord.x, coord.y].Exists(x => x.Item1 == "wall"))
            {
                AddToGrids(coord, element, true, true);
                agentMap[coord.x, coord.y].Find(x => x.Item1 == element).Item2.SetActive(false);
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

            if (updateAgentMap && !agentMap[coord.x, coord.y].Exists(x => x.Item1 == content || (x.Item1 == "visited" && content != "wall" && content != "danger")))
                ChangeColorMap(agentMap, coord, content, newColor);

            if (updatedMap && !map[coord.x, coord.y].Exists(x => x.Item1 == content))
                ChangeColorMap(map, coord, content, newColor);
        }
        else
        {
            if (updateAgentMap && content != "human")
                AddGameObjectMap(agentMap, coord, content, GetAgentMapOffset(coord));

            if (updatedMap && content != "human")
                AddGameObjectMap(map, coord, content, GetWorldMapOffset(coord));
        }

        void ChangeColorMap(List<(string, GameObject)>[,] _map, Vector2Int coord, string content, Color newColor)
        {
            _map[coord.x, coord.y].Add((content, null));
            _map[coord.x, coord.y].Find(x => x.Item1 == "cell").Item2.GetComponent<SpriteRenderer>().color = newColor;
        }

        void AddGameObjectMap(List<(string, GameObject)>[,] _map, Vector2Int coord, string content, Vector2 newPosition)
        {
            if (!_map[coord.x, coord.y].Exists(x => x.Item1 == content))
            {
                _map[coord.x, coord.y].Add((content, Instantiate(Resources.Load(content), transform) as GameObject));
                _map[coord.x, coord.y].Find(x => x.Item1 == content).Item2.transform.position = newPosition;
            }
            else
                _map[coord.x, coord.y].Find(x => x.Item1 == content).Item2.SetActive(true);
        }
    }

    public void RemoveFromGrids(Vector2Int coord, string content, bool updateAgentMap, bool updateMap)
    {
        if (updateAgentMap && agentMap[coord.x, coord.y].Exists(x => x.Item1 == content))
        {
            if (agentMap[coord.x, coord.y].Find(x => x.Item1 == content).Item2 != null)
                agentMap[coord.x, coord.y].Find(x => x.Item1 == content).Item2.SetActive(false);
            agentMap[coord.x, coord.y].Remove(agentMap[coord.x, coord.y].Find(x => x.Item1 == content));
            prologInterface.RemoveCellContentKB(coord, content);
        }

        if (updateMap && map[coord.x, coord.y].Exists(x => x.Item1 == content))
        {
            if (map[coord.x, coord.y].Find(x => x.Item1 == content).Item2 != null)
                map[coord.x, coord.y].Find(x => x.Item1 == content).Item2.SetActive(false);
            map[coord.x, coord.y].Remove(map[coord.x, coord.y].Find(x => x.Item1 == content));
        }
    }

    public void ShootArrow(Human agent, string direction)
    {
        prologInterface.RemoveFromKB("nb_arrow(_, _)");
        agent.nbArrow -= 1;
        prologInterface.AddToKB($"nb_arrow({agent.agentName}, {agent.nbArrow})", true);

        switch (direction)
        {
            case "right":
                for (int x = agent.coord.x; x < gridMax.x; x++)
                {
                    if (map[x, agent.coord.y].Exists(x => x.Item1 == "wumpus"))
                    {
                        KillWumpus(new Vector2Int(x, agent.coord.y));
                        break;
                    }
                }
                break;
            case "left":
                for (int x = agent.coord.x; x > gridMin.x; x--)
                {
                    if (map[x, agent.coord.y].Exists(x => x.Item1 == "wumpus"))
                    {
                        KillWumpus(new Vector2Int(x, agent.coord.y));
                        break;
                    }
                }
                break;
            case "up":
                for (int y = agent.coord.y; y < gridMax.y; y++)
                {
                    if (map[agent.coord.x, y].Exists(x => x.Item1 == "wumpus"))
                    {
                        KillWumpus(new Vector2Int(agent.coord.x, y));
                        break;
                    }
                }
                break;
            case "down":
                for (int y = agent.coord.y; y > gridMin.y; y--)
                {
                    if (map[agent.coord.x, y].Exists(x => x.Item1 == "wumpus"))
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
            prologInterface.AddCellContentKB(coordWumpus, "wumpusdead");
            gameController.MakeInferences();
        }
    }

    public void Move(Human agent, Vector2Int newCoord)
    {
        RemoveFromGrids(agent.coord, agent.agentName, true, true);

        if (!map[newCoord.x, newCoord.y].Exists(x => x.Item1 == agent.agentName))
        {
            map[newCoord.x, newCoord.y].Add((agent.agentName, null));
            agent.worldMapPrefab.transform.position = GetAgentMapOffset(newCoord);
        }
        if (!agentMap[newCoord.x, newCoord.y].Exists(x => x.Item1 == agent.agentName))
        {
            agentMap[newCoord.x, newCoord.y].Add((agent.agentName, null));
            agent.agentMapPrefab.transform.position = GetAgentMapOffset(newCoord);
        }

        agent.Move(newCoord);

        prologInterface.AddCellContentKB(agent.coord, agent.agentName);
        AddToGrids(agent.coord, "visited", true, false);
    }

    public Vector2 GetAgentMapOffset(Vector2Int coord)
    {
        return new Vector2((coord.x - gridMax.x / 2 - 0.65f) * tileSize, coord.y * tileSize);
    }

    public Vector2 GetWorldMapOffset(Vector2Int coord)
    {
        return new Vector2((coord.x + gridMax.x / 2 + 0.65f) * tileSize, coord.y * tileSize);
    }

}