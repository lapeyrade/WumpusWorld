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

    public GameObject gridManager;
    public GameObject mainCamera;
    private GameController gameController;
    private CameraController cameraController;
    private PrologInterface prologInterface;
    private Human human;
    // private Human human2;
    private Dog dog;

    void Awake()
    {
        gameController = gridManager.GetComponent<GameController>();
        prologInterface = gridManager.GetComponent<PrologInterface>();
        human = gridManager.GetComponent<Human>();
        // human2 = gridManager.GetComponent<Human>();
        dog = gridManager.GetComponent<Dog>();
    }

    void Start()
    {
        Random.InitState(RandomSeed); // Random Seed
        gridMax = new Coordinates(maxGridCol + 2, maxGridRow + 2);
        gridMin = new Coordinates(minGridCol, minGridRow);
        startCoords = new Coordinates(startCol, startRow);

        map = new Dictionary<string, GameObject>[gridMax.col, gridMax.row];


        if ((nbPit + nbWumpus + nbGold + nbAgent) > (gridMax.col * gridMax.row))
        {
            Debug.LogError("Map too small.");
            gameController.SetGameOver("Incorrect Parameters", true);
        }
        else
        {
            GenerateHuman();
            // GenerateDog();
            GenerateGrid();
            prologInterface.InitialiseGameKB(gridMin, gridMax, nbGold, nbWumpus, human);
            GrenerateWall();
            GenerateGold();
            GenerateWumpus();
            GeneratePit();

            InitialiseGame();
        }

        cameraController = mainCamera.GetComponent<CameraController>();
        cameraController.AdjustCameraPosition();
    }

    private void GenerateHuman()
    {
        human.initAgent(startCoords, "human", nbWumpus, gridMax);
    }

    private void GenerateGrid()
    {
        for (int col = gridMin.col; col < gridMax.col; col++)
        {
            for (int row = gridMin.row; row < gridMax.row; row++)
            {
                human.map[col, row] = new Dictionary<string, GameObject>();
                map[col, row] = new Dictionary<string, GameObject>();
                AddToGrids(col, row, "cell", true, true);
            }
        }
        AddToGrids(startCoords.col, startCoords.row, "start", true, true);
    }
    private void GrenerateWall()
    {
        for (int row = gridMin.row; row < gridMax.row; row++) // Right
        {
            WallGeneration(gridMax.col - 1, row);
        }
        for (int row = gridMin.row; row < gridMax.row; row++) // Left
        {
            WallGeneration(gridMin.row, row);
        }
        for (int col = gridMin.row; col < gridMax.col; col++) // Top
        {
            WallGeneration(col, gridMax.row - 1);
        }
        for (int col = gridMin.row; col < gridMax.col; col++) // Bottom
        {
            WallGeneration(col, gridMin.row);
        }

        void WallGeneration(int col, int row)
        {
            AddToGrids(col, row, "wall", false, true);
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
            human.map[col, row]["gold"].SetActive(false);
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
            human.map[col, row]["wumpus"].SetActive(false);

            AddToGrids(col, row, "wumpusdead", true, true);
            map[col, row]["wumpusdead"].SetActive(false);
            human.map[col, row]["wumpusdead"].SetActive(false);

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
            human.map[col, row]["pit"].SetActive(false);
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
                human.map[col, row][element].SetActive(false);

                if (map[col, row].ContainsKey("wall"))
                    map[col, row][element].SetActive(false);
            }
        }
    }

    private void InitialiseGame()
    {
        AddToGrids(startCoords.col, startCoords.row, "human", true, true);
        human.Move(startCoords);
        gameController.SenseCell(human);
        gameController.ActionCell(human);
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
            case "unknow": // Orange
                newColor = new Color(1.0f, 0.64f, 0.0f);
                break;
            case "visited": // Cyan
                newColor = Color.cyan;
                break;
            default:
                break;
        }

        if (content == "wall" || content == "safe" || content == "visited"  || content == "danger" || content == "unknow")
        {
            if (updateMapAgent && human.map[col, row].ContainsKey(content) == false)
            {
                human.map[col, row].Add(content, null);
                human.map[col, row]["cell"].GetComponent<SpriteRenderer>().color = newColor;
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
                if (human.map[col, row].ContainsKey(content) == false)
                {
                    human.map[col, row].Add(content, (GameObject)Instantiate(Resources.Load(content), transform));
                    human.map[col, row][content].transform.position = new Vector2((col - gridMax.col / 2 - 0.5f) * tileSize, row * tileSize);
                }
                else
                    human.map[col, row][content].SetActive(true);
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

    public void RemoveFromGrids(int col, int row, string content, bool updateMapAgent, bool updateMap)
    {
        if (updateMapAgent && human.map[col, row].ContainsKey(content) == true)
        {
            human.map[col, row][content].SetActive(false);
            human.map[col, row].Remove(content);
            prologInterface.RemoveCellContentKB(new Coordinates(col, row), content);
        }

        if (updateMap && map[col, row].ContainsKey(content) == true)
        {
            map[col, row][content].SetActive(false);
            map[col, row].Remove(content);
        }
    }

    public void ShootArrow(string direction)
    {
        Debug.Log("Shooting " + direction);
        human.nbArrow--;
        human.nbArrowUsed++;
        prologInterface.RemoveFromKB("nb_arrow(_)");
        prologInterface.RemoveFromKB("nb_arrow_used(_)");
        prologInterface.AddToKB($"nb_arrow({human.nbArrow})", true);
        prologInterface.AddToKB($"nb_arrow_used({human.nbArrowUsed})", true);

        switch (direction)
        {
            case "right":
                for (int col = human.coords.col; col < gridMax.col; col++)
                {
                    if (map[col, human.coords.row].ContainsKey("wumpus"))
                    {
                        KillWumpus(new Coordinates(col, human.coords.row));
                        break;
                    }
                }
                break;
            case "left":
                for (int col = human.coords.col; col > gridMin.col; col--)
                {
                    if (map[col, human.coords.row].ContainsKey("wumpus"))
                    {
                        KillWumpus(new Coordinates(col, human.coords.row));
                        break;
                    }
                }
                break;
            case "up":
                for (int row = human.coords.row; row < gridMax.row; row++)
                {
                    if (map[human.coords.col, row].ContainsKey("wumpus"))
                    {
                        KillWumpus(new Coordinates(human.coords.col, row));
                        break;
                    }
                }
                break;
            case "down":
                for (int row = human.coords.row; row > gridMin.row; row--)
                {
                    if (map[human.coords.col, row].ContainsKey("wumpus"))
                    {
                        KillWumpus(new Coordinates(human.coords.col, row));
                        break;
                    }
                }
                break;
        }

        void KillWumpus(Coordinates coords)
        {
            human.nbWumpusDead++;
            prologInterface.RemoveFromKB("nb_wumpus_dead(_)");
            prologInterface.AddToKB($"nb_wumpus_dead({human.nbWumpusDead})", true);

            RemoveFromGrids(coords.col, coords.row, "wumpus", true, true);
            AddToGrids(coords.col, coords.row, "wumpusdead", true, true);
            prologInterface.AddToKB($"cell({coords.col}, {coords.row}, wumpusdead)", true);
            gameController.makeInferences();
        }
    }
}