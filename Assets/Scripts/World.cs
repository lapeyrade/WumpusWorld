using UnityEngine;
using System.Collections.Generic;

/// <summary>
/// Construct and update the world
/// Manage the graphical part
/// </summary>
public class World : MonoBehaviour
{
    [SerializeField] private int RandomSeed = 29;
    private int minGridCol = 0;
    private int minGridRow = 0;

    [SerializeField] private int maxGridCol = 7;
    [SerializeField] private int maxGridRow = 7;
    [SerializeField] private int startCol = 1;
    [SerializeField] private int startRow = 1;
    [SerializeField] public float tileSize = 1.05f;
    [SerializeField] private int nbPit = 4;
    [SerializeField] private int nbWumpus = 2;
    [SerializeField] public int nbGold = 1;
    [SerializeField] private int nbAgent = 2;
    public Coordinates gridMin;
    public Coordinates gridMax;
    private Coordinates startCoords;

    public Dictionary<string, GameObject>[,] map;
    public Dictionary<string, GameObject>[,] agentMap;

    public GameObject gridManager;
    public GameObject mainCamera;
    private GameController gameController;
    private CameraController cameraController;
    private PrologInterface prologInterface;
    public Human human;
    public Human human2;
    private Dog dog;

    void Awake()
    {
        gameController = gridManager.GetComponent<GameController>();
        prologInterface = gridManager.GetComponent<PrologInterface>();
    }

    void Start()
    {
        Random.InitState(RandomSeed); // Random Seed
        gridMax = new Coordinates(maxGridCol + 2, maxGridRow + 2);
        gridMin = new Coordinates(minGridCol, minGridRow);
        startCoords = new Coordinates(startCol, startRow);

        map = new Dictionary<string, GameObject>[gridMax.col, gridMax.row];
        agentMap = new Dictionary<string, GameObject>[gridMax.col, gridMax.row];


        if ((nbPit + nbWumpus + nbGold + nbAgent) > (gridMax.col * gridMax.row))
        {
            Debug.LogError("Map too small.");
            gameController.SetGameOver("Incorrect Parameters", true);
        }
        else
        {
            GenerateGrid();
            GenerateHuman();
            // GenerateDog();
            prologInterface.InitialiseGameKB(human, human2, nbWumpus);
            GrenerateWall();
            GenerateGold();
            GenerateWumpus();
            GeneratePit();

            InitialiseGame();
        }

        cameraController = mainCamera.GetComponent<CameraController>();
        cameraController.AdjustCameraPosition();
    }

    private void GenerateGrid()
    {
        for (int col = gridMin.col; col < gridMax.col; col++)
        {
            for (int row = gridMin.row; row < gridMax.row; row++)
            {
                agentMap[col, row] = new Dictionary<string, GameObject>();
                map[col, row] = new Dictionary<string, GameObject>();
                AddToGrids(col, row, "cell", true, true);
            }
        }
        AddToGrids(startCoords.col, startCoords.row, "start", true, true);
    }

    private void GenerateHuman()
    {
        human = new Human(1, "human", startCoords, nbWumpus);
        AddToGrids(human.coords.col, human.coords.row, human.name, true, true);
        human2 = new Human(2, "human", new Coordinates(maxGridCol - 1, maxGridRow - 1), nbWumpus);
        AddToGrids(human2.coords.col, human2.coords.row, human2.name, true, true);
    }

    private void GrenerateWall()
    {
        for (int row = gridMin.row; row < gridMax.row; row++) // Right
        {
            AddToGrids(gridMax.col - 1, row, "wall", false, true);
        }
        for (int row = gridMin.row; row < gridMax.row; row++) // Left
        {
            AddToGrids(gridMin.row, row, "wall", false, true);
        }
        for (int col = gridMin.row; col < gridMax.col; col++) // Top
        {
            AddToGrids(col, gridMax.row - 1, "wall", false, true);
        }
        for (int col = gridMin.row; col < gridMax.col; col++) // Bottom
        {
            AddToGrids(col, gridMin.row, "wall", false, true);
        }
    }

    private void GenerateGold()
    {
        int col = Random.Range(gridMin.col, gridMax.col - 1);
        int row = Random.Range(gridMin.row, gridMax.row - 1);
        for (int gold = 0; gold < nbGold; gold++)
        {
            while (map[col, row].ContainsKey("start") || map[col, row].ContainsKey("pit") || map[col, row].ContainsKey("wumpus") || map[col, row].ContainsKey("gold") || map[col, row].ContainsKey("wall"))
            {
                col = Random.Range(gridMin.col, gridMax.col - 1);
                row = Random.Range(gridMin.row, gridMax.row - 1);
            }
            AddToGrids(col, row, "gold", true, true);
            agentMap[col, row]["gold"].SetActive(false);
        }
    }

    private void GenerateWumpus()
    {
        int col = Random.Range(gridMin.col, gridMax.col - 1);
        int row = Random.Range(gridMin.row, gridMax.row - 1);
        for (int wumpus = 0; wumpus < nbWumpus; wumpus++)
        {
            while (map[col, row].ContainsKey("start") || map[col, row].ContainsKey("pit") || map[col, row].ContainsKey("wumpus") || map[col, row].ContainsKey("gold") || map[col, row].ContainsKey("wall") || (col == startCoords.col && row == startCoords.row + 1) || (col == startCoords.col + 1 && row == startCoords.row))
            {
                col = Random.Range(gridMin.col, gridMax.col - 1);
                row = Random.Range(gridMin.row, gridMax.row - 1);
            }
            AddToGrids(col, row, "wumpus", true, true);
            agentMap[col, row]["wumpus"].SetActive(false);

            AddToGrids(col, row, "wumpusdead", true, true);
            map[col, row]["wumpusdead"].SetActive(false);
            agentMap[col, row]["wumpusdead"].SetActive(false);

            GenerateAroundCell(col, row, "stenchyes");  // Stench Generation
        }
    }

    private void GeneratePit()
    {
        int col = Random.Range(gridMin.col, gridMax.col - 1);
        int row = Random.Range(gridMin.row, gridMax.row - 1);
        for (int pit = 0; pit < nbPit; pit++)
        {
            while (map[col, row].ContainsKey("start") || map[col, row].ContainsKey("pit") || map[col, row].ContainsKey("wumpus") || map[col, row].ContainsKey("gold") || map[col, row].ContainsKey("wall") || (col == startCoords.col && row == startCoords.row + 1) || (col == startCoords.col + 1 && row == startCoords.row))
            {
                col = Random.Range(gridMin.col, gridMax.col - 1);
                row = Random.Range(gridMin.row, gridMax.row - 1);
            }
            AddToGrids(col, row, "pit", true, true);
            agentMap[col, row]["pit"].SetActive(false);
            GenerateAroundCell(col, row, "breezeyes");  // yes Generation
        }
    }

    private void GenerateAroundCell(int col, int row, string element)
    {
        Generate(col + 1, row); // Right cell
        Generate(col - 1, row); // Left cell
        Generate(col, row + 1); // Top cell
        Generate(col, row - 1); // Bottom cell

        void Generate(int col, int row)
        {
            if (!map[col, row].ContainsKey(element))
            {
                AddToGrids(col, row, element, true, true);
                agentMap[col, row][element].SetActive(false);

                if (map[col, row].ContainsKey("wall"))
                    map[col, row][element].SetActive(false);
            }
        }
    }

    private void InitialiseGame()
    {
        Move(human, human.coords);
        gameController.SenseCell(human);
        gameController.ActionCell(human);

        Move(human2, human2.coords);
        gameController.SenseCell(human2);
        gameController.ActionCell(human2);

        prologInterface.PrintKB(human);
    }

    public void AddToGrids(int col, int row, string content, bool updateMapAgent, bool updateMap)
    {
        Color newColor = Color.white;

        switch (content)
        {
            case "safe": // Dark Green
                newColor = new Color(0.145f, 0.701f, 0.294f, 1);
                break;
            case "wall": // Black 
                newColor = Color.black;
                break;
            case "danger": // Red
                newColor = Color.red;
                break;
            case "undefined": // Orange
                newColor = new Color(1.0f, 0.64f, 0.0f);
                break;
            case "visited": // Cyan
                newColor = Color.cyan;
                break;
            default:
                break;
        }

        if (content == "wall" || content == "safe" || content == "visited" || content == "danger" || content == "undefined")
        {
            if (updateMapAgent && agentMap[col, row].ContainsKey(content) == false)
            {
                agentMap[col, row].Add(content, null);
                agentMap[col, row]["cell"].GetComponent<SpriteRenderer>().color = newColor;
            }

            if (updateMap && map[col, row].ContainsKey(content) == false)
            {
                map[col, row].Add(content, null);
                map[col, row]["cell"].GetComponent<SpriteRenderer>().color = newColor;
            }
        }
        else
        {
            if (updateMapAgent)
            {
                if (agentMap[col, row].ContainsKey(content) == false)
                {
                    agentMap[col, row].Add(content, (GameObject)Instantiate(Resources.Load(content), transform));
                    agentMap[col, row][content].transform.position = new Vector2((col - gridMax.col / 2 - 0.5f) * tileSize, row * tileSize);
                }
                else
                    agentMap[col, row][content].SetActive(true);
            }
            if (updateMap)
            {
                if (map[col, row].ContainsKey(content) == false)
                {
                    map[col, row].Add(content, (GameObject)Instantiate(Resources.Load(content), transform));
                    map[col, row][content].transform.position = new Vector2((col + gridMax.col / 2 + 0.5f) * tileSize, row * tileSize);
                }
                else
                    map[col, row][content].SetActive(true);
            }
        }
    }

    public void RemoveFromGrids(Human agent, int col, int row, string content, bool updateMapAgent, bool updateMap)
    {
        if (updateMapAgent && agentMap[col, row].ContainsKey(content) == true)
        {
            agentMap[col, row][content].SetActive(false);
            agentMap[col, row].Remove(content);
            prologInterface.RemoveCellContentKB(agent, new Coordinates(col, row), content);
        }

        if (updateMap && map[col, row].ContainsKey(content) == true)
        {
            map[col, row][content].SetActive(false);
            map[col, row].Remove(content);
        }
    }

    public void ShootArrow(Human agent, string direction)
    {
        Debug.Log("Shooting " + direction);
        prologInterface.RemoveFromKB(agent, "nb_arrow(_, _)");
        agent.nbArrow -= 1;
        prologInterface.AddToKB(agent, $"nb_arrow({agent.name}, {agent.nbArrow})", true);

        switch (direction)
        {
            case "right":
                for (int col = agent.coords.col; col < gridMax.col; col++)
                {
                    if (map[col, agent.coords.row].ContainsKey("wumpus"))
                    {
                        KillWumpus(agent, new Coordinates(col, agent.coords.row));
                        break;
                    }
                }
                break;
            case "left":
                for (int col = agent.coords.col; col > gridMin.col; col--)
                {
                    if (map[col, agent.coords.row].ContainsKey("wumpus"))
                    {
                        KillWumpus(agent, new Coordinates(col, agent.coords.row));
                        break;
                    }
                }
                break;
            case "up":
                for (int row = agent.coords.row; row < gridMax.row; row++)
                {
                    if (map[agent.coords.col, row].ContainsKey("wumpus"))
                    {
                        KillWumpus(agent, new Coordinates(agent.coords.col, row));
                        break;
                    }
                }
                break;
            case "down":
                for (int row = agent.coords.row; row > gridMin.row; row--)
                {
                    if (map[agent.coords.col, row].ContainsKey("wumpus"))
                    {
                        KillWumpus(agent, new Coordinates(agent.coords.col, row));
                        break;
                    }
                }
                break;
        }

        void KillWumpus(Human agent, Coordinates coordsWumpus)
        {
            RemoveFromGrids(agent, coordsWumpus.col, coordsWumpus.row, "wumpus", true, true);
            AddToGrids(coordsWumpus.col, coordsWumpus.row, "wumpusdead", true, true);
            prologInterface.AddCellContentKB(agent, coordsWumpus, "wumpusdead");
            gameController.makeInferences(agent);
        }
    }

    public void Move(Human agent, Coordinates newCoords)
    {
        RemoveFromGrids(agent, agent.coords.col, agent.coords.row, agent.name, true, true);
        agent.Move(newCoords);
        AddToGrids(agent.coords.col, agent.coords.row, agent.name, true, true);
        AddToGrids(agent.coords.col, agent.coords.row, "visited", true, false);
    }
}