using UnityEngine;
using System.Collections.Generic;

/// <summary>
/// Construct and update the world
/// Manage the graphical part 
/// </summary>
public class World : MonoBehaviour
{
    [SerializeField]
    private int RandomSeed = 29;

    private int minGridCol = 0;
    private int minGridRow = 0;
    [SerializeField]
    private int maxGridCol = 7;
    [SerializeField]
    private int maxGridRow = 7;
    [SerializeField]
    private int startCol = 1;
    [SerializeField]
    private int startRow = 1;
    [SerializeField]
    public float tileSize = 1.05f;
    [SerializeField]
    private int nbPit = 4;
    [SerializeField]
    private int nbWumpus = 2;
    [SerializeField]
    public int nbGold = 1;
    [SerializeField]
    private int nbAgent = 1;

    public Coordinates gridMin;
    public Coordinates gridMax;
    private Coordinates startCoords;

    public Dictionary<string, GameObject>[,] map;

    public GameObject gridManager;
    public GameObject mainCamera;
    private GameController gameController;
    private CameraController cameraController;
    private PrologInterface prologInterface;
    private Agent agent;


    void Awake()
    {
        gameController = gridManager.GetComponent<GameController>();
        prologInterface = gridManager.GetComponent<PrologInterface>();
        agent = gridManager.GetComponent<Agent>();
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
            GenerateAgent();
            GenerateGrid();
            prologInterface.InitialiseGameKB(gridMin, gridMax, nbGold, nbWumpus);
            GrenerateWall();
            GenerateGold();
            GenerateWumpus();
            GeneratePit();

            InitialiseGame();
        }

        cameraController = mainCamera.GetComponent<CameraController>();
        cameraController.AdjustCameraPosition();
    }

    private void GenerateAgent()
    {
        agent.InitAgent(startCoords, nbWumpus, gridMax);
        if (nbAgent == 1)
        {
        }
        else if (nbAgent > 1)
        {
            int col = startCoords.col;
            int row = startCoords.row;
            for (int agent = 0; agent < nbAgent; agent++)
            {
                while (map[col, row].ContainsKey("start") || map[col, row].ContainsKey("pit") || map[col, row].ContainsKey("wumpus") || map[col, row].ContainsKey("gold"))
                {
                    col = Random.Range(gridMin.col, gridMax.col);
                    row = Random.Range(gridMin.row, gridMax.row);
                }
                AddToGrids(col, row, "agent", true, true);
            }
        }
    }

    private void GenerateGrid()
    {
        for (int col = gridMin.col; col < gridMax.col; col++)
        {
            for (int row = gridMin.row; row < gridMax.row; row++)
            {
                agent.map[col, row] = new Dictionary<string, GameObject>();
                map[col, row] = new Dictionary<string, GameObject>();
                if (!(col == startCoords.col && row == startCoords.row))
                    AddToGrids(col, row, "emptyCell", true, true);
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
            RemoveFromGrids(col, row, "emptyCell", false, true);
            RemoveFromGrids(col, row, "safe", false, true);
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
            AddToGrids(col, row, "gold", false, true);
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
            AddToGrids(col, row, "wumpus", false, true);
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
            AddToGrids(col, row, "pit", false, true);
            RemoveFromGrids(col, row, "emptyCell", false, true);
            GenerateAroundCell(col, row, "breezeyes");  // yes Generation
        }
    }


    private void GenerateAroundCell(int col, int row, string keyGenerated)
    {
        Generate(col + 1, row); // Right cell
        Generate(col - 1, row); // Left cell
        Generate(col, row + 1); // Top cell
        Generate(col, row - 1); // Bottom cell

        void Generate(int col, int row)
        {
            if (!map[col, row].ContainsKey(keyGenerated) && !map[col, row].ContainsKey("pit") && !map[col, row].ContainsKey("wall"))
            {
                AddToGrids(col, row, keyGenerated, false, true);
            }
        }
    }

    private void InitialiseGame()
    {
        AddToGrids(startCoords.col, startCoords.row, "agent", true, true);
        agent.Move(startCoords);
        gameController.SenseCell();
        gameController.ActionCell();
        prologInterface.PrintKB(agent);
    }

    public void AddToGrids(int col, int row, string content, bool updateMapAgent, bool updateMap)
    {
        if (updateMapAgent && agent.map[col, row].ContainsKey(content) == false)
        {
            agent.map[col, row].Add(content, (GameObject)Instantiate(Resources.Load(content), transform));
            agent.map[col, row][content].transform.position = new Vector2((col - gridMax.col / 2 - 1f) * tileSize, row * tileSize);
        }

        if (updateMap && map[col, row].ContainsKey(content) == false)
        {
            map[col, row].Add(content, (GameObject)Instantiate(Resources.Load(content), transform));
            map[col, row][content].transform.position = new Vector2((col + gridMax.col / 2 + 1f) * tileSize, row * tileSize);

        }
    }

    public void RemoveFromGrids(int col, int row, string content, bool updateMapAgent, bool updateMap)
    {
        if (updateMapAgent && agent.map[col, row].ContainsKey(content) == true)
        {
            // Destroy(agent.map[col, row][content]);
            agent.map[col, row][content].SetActive(false);
            agent.map[col, row].Remove(content);
            prologInterface.RemoveCellContentKB(new Coordinates(col, row), content);
        }

        if (updateMap && map[col, row].ContainsKey(content) == true)
        {
            Destroy(map[col, row][content]);
            map[col, row].Remove(content);
        }
    }

    public void ShotArrow(string direction)
    {
        switch(direction)
        {
            case "right":
                for(int col = agent.coords.col; col < gridMax.col; col++)
                    KillWumpus(new Coordinates(col, agent.coords.row));
                break;
            case "left":
                for(int col = gridMin.col; col < agent.coords.col; col++)
                    KillWumpus(new Coordinates(col, agent.coords.row));
                break;
            case "up":
                for(int row = agent.coords.row; row < gridMax.row; row++)
                    KillWumpus(new Coordinates(agent.coords.col, row));
                break;
            case "down":
                for(int row = gridMin.row; row < agent.coords.row; row++)
                    KillWumpus(new Coordinates(agent.coords.col, row));
                break;
        }
    }

    void KillWumpus(Coordinates coords)
    {
        if(map[coords.col, coords.row].ContainsKey("wumpus"))
            {
                Debug.Log("Shooting");
                RemoveFromGrids(coords.col, coords.row, "wumpus", true, true);
                AddToGrids(coords.col, coords.row, "wumpusDead", true, true);
                prologInterface.AddToKB($"cell({coords.col}, {coords.row}, wumpusDead)");
            }
    }
}