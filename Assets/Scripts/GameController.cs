using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;

public class GameController : MonoBehaviour
{
    
    [SerializeField]
    private int RandomSeed = 5;

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
    private int nbPit = 3;
    [SerializeField]
    private int nbWumpus = 1;
    [SerializeField]
    private int nbGold = 1;
    [SerializeField]
    private int nbAgent = 1;

    [SerializeField]
    private bool debugEnabled = true;
    
    public Coordinates gridMax;
    public Coordinates gridMin;
    private Coordinates startCoords;
    private Dictionary<string, GameObject>[,] map;
    private Dictionary<string, GameObject>[,] mapFull;

    private bool gameOver = false;
    private bool useProlog = true;

    private Action action;
    private Agent agent;


    void Awake()
    {
        Random.InitState(RandomSeed); // Random Seed

        gridMax = new Coordinates(maxGridCol + 2, maxGridRow + 2);
        gridMin = new Coordinates(minGridCol, minGridRow);
        startCoords = new Coordinates(startCol, startRow);
        action = new Action(nbWumpus, nbGold, startCoords, debugEnabled);

        map = new Dictionary<string, GameObject>[gridMax.col, gridMax.row];
        mapFull = new Dictionary<string, GameObject>[gridMax.col, gridMax.row];

        if ((nbPit + nbWumpus + nbGold + nbAgent) > (gridMax.col * gridMax.row))
        {
            Debug.LogError("Map too small.");
            gameOver = true;
        }
        else
        {
            GenerateGrid();
            if(useProlog)
            {
                action.InitialiseGameKB(RandomSeed, startCoords, gridMin, gridMax, nbGold, nbWumpus, nbPit);
            }
            GrenerateWall();
            GenerateGold();
            GenerateWumpus();
            GeneratePit();
            GenerateAgent();
        }
    }

    private void GenerateGrid()
    {
        for (int col = gridMin.col; col < gridMax.col; col++)
        {
            for (int row = gridMin.row; row < gridMax.row ; row++) 
            {
                map[col, row] = new Dictionary<string, GameObject>();
                mapFull[col, row] = new Dictionary<string, GameObject>();
                if(!(col == startCoords.col && row == startCoords.row))
                    AddToGrids(col, row, "emptyCell", true, true);
            }
        }
        AddToGrids(startCoords.col, startCoords.row, "start", true, true);
    }

    private void GrenerateWall()
    {
        if(useProlog)
        {
            List<Coordinates> wallsCoords = action.CoordinatesState("wall");
            foreach(Coordinates coords in wallsCoords)
            {
                WallGeneration(coords.col, coords.row);
            }
        }
        else{
            for (int row = gridMin.row; row < gridMax.row; row++) // right
            {
                WallGeneration(gridMax.col - 1, row);
            }
            for (int row = gridMin.row; row < gridMax.row; row++) // left
            {
                WallGeneration(gridMin.row, row);
            }
            for (int col = gridMin.row; col < gridMax.col; col++) // top
            {
                WallGeneration(col, gridMax.row - 1);
            }
            for (int col = gridMin.row; col < gridMax.col; col++) // bottom
            {
                WallGeneration(col, gridMin.row);
            }
        }

        void WallGeneration(int col, int row)
        {
            RemoveFromGrids(col, row, "emptyCell", false, true);
            RemoveFromGrids(col, row, "safe", false, true);
            AddToGrids(col, row, "wall", false, true);
            // action.AddFactKB($"world({col}, {row}, wall)");
        }
    }

    private void GenerateGold()
    {
        if(useProlog)
        {
            List<Coordinates> goldsCoords = action.CoordinatesState("gold");
            foreach(Coordinates coords in goldsCoords)
            {
                AddToGrids(coords.col, coords.row, "gold", false, true);
            }
        }
        else
        {
            int col = Random.Range(gridMin.col, gridMax.col - 1);
            int row = Random.Range(gridMin.row, gridMax.row - 1);
            for (int gold = 0; gold < nbGold; gold++)
            {
                while (mapFull[col, row].ContainsKey("start") || mapFull[col, row].ContainsKey("pit") || mapFull[col, row].ContainsKey("wumpus") || mapFull[col, row].ContainsKey("gold") ||mapFull[col, row].ContainsKey("wall"))
                {
                    col = Random.Range(gridMin.col, gridMax.col - 1);
                    row = Random.Range(gridMin.row, gridMax.row - 1);
                }
                AddToGrids(col, row, "gold", false, true);
            }
        }
    }

    private void GenerateWumpus()
    {
        if(useProlog)
        {
            List<Coordinates> wumpusCoords = action.CoordinatesState("wumpus");
            foreach(Coordinates coords in wumpusCoords)
            {
                AddToGrids(coords.col, coords.row, "wumpus", false, true);
                GenerateAroundCell(coords.col, coords.row, "stench");  // Stench Generation
            }
        }
        else
        {
            int col = Random.Range(gridMin.col, gridMax.col - 1);
            int row = Random.Range(gridMin.row, gridMax.row - 1);
            for (int wumpus = 0; wumpus < nbWumpus; wumpus++)
            {
                while (mapFull[col, row].ContainsKey("start") || mapFull[col, row].ContainsKey("pit") || mapFull[col, row].ContainsKey("wumpus") || mapFull[col, row].ContainsKey("gold") || mapFull[col, row].ContainsKey("wall") || (col == startCol && row == startRow + 1) || (col == startCol + 1 && row == startRow))
                {
                    col = Random.Range(gridMin.col, gridMax.col - 1);
                    row = Random.Range(gridMin.row, gridMax.row - 1);
                }
                AddToGrids(col, row, "wumpus", false, true);
                GenerateAroundCell(col, row, "stench");  // Stench Generation
            }
        }
    }

    private void GeneratePit()
    {
        if(useProlog)
        {
            List<Coordinates> pitCoords = action.CoordinatesState("pit");
            foreach(Coordinates coords in pitCoords)
            {
                AddToGrids(coords.col, coords.row, "pit", false, true);
                RemoveFromGrids(coords.col, coords.row, "emptyCell", false, true);
                GenerateAroundCell(coords.col, coords.row, "breeze");  // breeze Generation
            }
        }
        else
        {
            int col = Random.Range(gridMin.col, gridMax.col - 1);
            int row = Random.Range(gridMin.row, gridMax.row - 1);
            for (int pit = 0; pit < nbPit; pit++)
            {
                while (mapFull[col, row].ContainsKey("start") || mapFull[col, row].ContainsKey("pit") || mapFull[col, row].ContainsKey("wumpus") || mapFull[col, row].ContainsKey("gold") || mapFull[col, row].ContainsKey("wall") || (col == startCol && row == startRow + 1) || (col == startCol + 1 && row == startRow))
                {
                    col = Random.Range(gridMin.col, gridMax.col - 1);
                    row = Random.Range(gridMin.row, gridMax.row - 1);
                } 
                AddToGrids(col, row, "pit", false, true);
                RemoveFromGrids(col, row, "emptyCell", false, true);
                GenerateAroundCell(col, row, "breeze");  // breeze Generation
            }
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
            if (!mapFull[col, row].ContainsKey(keyGenerated) && !mapFull[col, row].ContainsKey("pit") && !mapFull[col, row].ContainsKey("wall"))
            {
                AddToGrids(col, row, keyGenerated, false, true);
            }
        }
    }

    private void GenerateAgent()
    {
        if (nbAgent == 1)
        {
            AddToGrids(startCoords.col, startCoords.row, "agent", true, true);
            agent = new Agent(startCoords, nbWumpus);
            CheckNearCells(startCoords);
        }
        else if (nbAgent > 1)
        {
            int col = startCoords.col;
            int row = startCoords.row;
            for (int agent = 0; agent < nbAgent; agent++)
            {
                while (mapFull[col, row].ContainsKey("start") || mapFull[col, row].ContainsKey("pit") || mapFull[col, row].ContainsKey("wumpus") || mapFull[col, row].ContainsKey("gold"))
                {
                    col = Random.Range(gridMin.col, gridMax.col);
                    row = Random.Range(gridMin.row, gridMax.row);
                }
                AddToGrids(col, row, "agent", true, true);
            }
        }
    }

    private void AddToGrids(int col, int row, string cell, bool updateMap, bool updateMapFull)
    {
        if(updateMap && map[col, row].ContainsKey(cell) == false)
        {
            map[col, row].Add(cell, (GameObject)Instantiate(Resources.Load(cell), transform));
            map[col, row][cell].transform.position = new Vector2((col - gridMax.col / 2 - 1f) * tileSize, row * tileSize);
        }

        if(updateMapFull && mapFull[col, row].ContainsKey(cell) == false)
        {
            mapFull[col, row].Add(cell, (GameObject)Instantiate(Resources.Load(cell), transform));
            mapFull[col, row][cell].transform.position = new Vector2((col + gridMax.col /2 + 1f) * tileSize, row * tileSize);

        }
    }

    private void RemoveFromGrids(int col, int row, string cell, bool updateMap, bool updateMapFull)
    {
        if(updateMap && map[col, row].ContainsKey(cell) == true)
        {
            Destroy(map[col, row][cell]);
            map[col, row].Remove(cell);
        }

        if(updateMapFull && mapFull[col, row].ContainsKey(cell) == true)
        {
            Destroy(mapFull[col, row][cell]);
            mapFull[col, row].Remove(cell);
        }
    }

    private void CheckNearCells(Coordinates coords)
    {
        if(!map[coords.col + 1, coords.row].ContainsKey("visited")) // right
        {
            foreach (string state in action.CheckCell(new Coordinates(coords.col + 1, coords.row)))
            {
                AddToGrids(coords.col + 1, coords.row, state, true, false);
                RemoveFromGrids(coords.col + 1, coords.row, "emptyCell", true, false);
            }
        }
        if(!map[coords.col - 1, coords.row].ContainsKey("visited")) // left
        {
            foreach (string state in action.CheckCell(new Coordinates(coords.col - 1, coords.row)))
            {
                AddToGrids(coords.col - 1, coords.row, state, true, false);
                RemoveFromGrids(coords.col - 1, coords.row, "emptyCell", true, false);
            }
        }
        if(!map[coords.col, coords.row + 1].ContainsKey("visited")) // top
        {
            foreach (string state in action.CheckCell(new Coordinates(coords.col, coords.row + 1)))
            {
                AddToGrids(coords.col, coords.row + 1, state, true, false);
                RemoveFromGrids(coords.col, coords.row + 1, "emptyCell", true, false);
            }
        }
        if(!map[coords.col, coords.row - 1].ContainsKey("visited")) // bottom
        {
            foreach (string state in action.CheckCell(new Coordinates(coords.col, coords.row - 1)))
            {
                AddToGrids(coords.col, coords.row - 1, state, true, false);
                RemoveFromGrids(coords.col, coords.row -1, "emptyCell", true, false);
            }
        }
    }


    private void MoveAgent(Coordinates newCoords)
    {
        if (!mapFull[newCoords.col, newCoords.row].ContainsKey("wall"))
        {
            action.RemoveFromKB($"cell({agent.coords.col}, {agent.coords.row}, agent)");
            action.AddFactKB($"cell({newCoords.col}, {newCoords.row}, agent)");
            action.AddFactKB($"cell({newCoords.col}, {newCoords.row}, visited)");

            RemoveFromGrids(newCoords.col, newCoords.row, "safe", true, true);
            RemoveFromGrids(newCoords.col, newCoords.row, "emptyCell", true, true);

            if(!map[newCoords.col, newCoords.row].ContainsKey("start"))
                AddToGrids(newCoords.col, newCoords.row, "visited", true, true);

            RemoveFromGrids(agent.coords.col, agent.coords.row, "agent", true, true);
            AddToGrids(newCoords.col, newCoords.row, "agent", true, true);

            if(agent.coords.col < newCoords.col)
            {
                map[newCoords.col, newCoords.row]["agent"].GetComponent<SpriteRenderer>().flipX = false;
                mapFull[newCoords.col, newCoords.row]["agent"].GetComponent<SpriteRenderer>().flipX = false;
            }
            else if(agent.coords.col > newCoords.col)
            {
                map[newCoords.col, newCoords.row]["agent"].GetComponent<SpriteRenderer>().flipX = true;
                mapFull[newCoords.col, newCoords.row]["agent"].GetComponent<SpriteRenderer>().flipX = true;
            }

            agent.coords.col = newCoords.col;
            agent.coords.row = newCoords.row;

            ActionCase(mapFull[agent.coords.col, agent.coords.row]);
        }
        else
        {
            action.BumpIntoWall();
            RemoveFromGrids(newCoords.col, newCoords.row, "emptyCell", true, false);
            RemoveFromGrids(newCoords.col, newCoords.row, "safe", true, false);
            AddToGrids(newCoords.col, newCoords.row, "wall", true, false);
            // action.RemoveFromKB($"cell({newCoords.col}, {newCoords.row}, safe)");
            // action.AddFactKB($"cell({newCoords.col}, {newCoords.row}, wall)");
            action.printKB();
        }
    }

    private void ActionCase(Dictionary<string, GameObject> position)
    {
        Coordinates coordsWumpus = action.WumpusFound();
        if (coordsWumpus != null && !map[coordsWumpus.col, coordsWumpus.row].ContainsKey("wumpusDead"))
        {
            if(mapFull[coordsWumpus.col, coordsWumpus.row].ContainsKey("wumpus"))
                AddToGrids(coordsWumpus.col, coordsWumpus.row, "wumpusDead", false, true);

            AddToGrids(coordsWumpus.col, coordsWumpus.row, "wumpusDead", true, false);
            RemoveFromGrids(coordsWumpus.col, coordsWumpus.row, "wumpus", true, true);
            AddToGrids(coordsWumpus.col, coordsWumpus.row, "safe", true, false);

            action.RemoveFromKB($"cell({coordsWumpus.col}, {coordsWumpus.row}, wumpus)");
            action.AddFactKB($"cell({coordsWumpus.col}, {coordsWumpus.row}, safe)");
            action.AddFactKB($"cell({coordsWumpus.col}, {coordsWumpus.row}, wumpusDead)");
            mapFull[coordsWumpus.col, coordsWumpus.row].Remove("wumpus");
            agent.nbArrow -= 1;
        }

        if (position.ContainsKey("pit") || position.ContainsKey("wumpus"))
            gameOver = true;

        if (position.ContainsKey("gold"))
        {
            RemoveFromGrids(agent.coords.col, agent.coords.row, "gold", false, true);

            action.AddFactKB($"cell({agent.coords.col}, {agent.coords.row}, gold)");
            action.RemoveFromKB($"goldAgent({agent.nbGold})");
            agent.nbGold += 1;
            action.AddFactKB($"goldAgent({agent.nbGold})");
            action.RemoveFromKB($"cell({agent.coords.col}, {agent.coords.row}, gold)");
            if (agent.nbGold == nbGold)
            {
                action.RemoveFromKB($"allGoldsFound(false)");
                action.AddFactKB($"allGoldsFound(true)");
            }
        }
        if (position.ContainsKey("breeze"))
        {
            AddToGrids(agent.coords.col, agent.coords.row, "breeze", true, false);
            // action.RemoveFromKB($"cell({agent.coords.col}, {agent.coords.row}, safe)");
            // action.AddFactKB($"cell({agent.coords.col}, {agent.coords.row}, breeze)");
        }
        if (position.ContainsKey("stench"))
        {
            AddToGrids(agent.coords.col, agent.coords.row, "stench", true, false);
            // action.RemoveFromKB($"cell({agent.coords.col}, {agent.coords.row}, safe)");
            // action.AddFactKB($"cell({agent.coords.col}, {agent.coords.row}, stench)");
        }
        if (position.ContainsKey("start"))
        {
            if (agent.nbGold == nbGold)
            {
                gameOver = true;
            }
        }
        action.CheckNearCells(new Coordinates(agent.coords.col, agent.coords.row));
        CheckNearCells(new Coordinates(agent.coords.col, agent.coords.row));

        action.printKB();
    }

    private void waitForAction()
    {
        if (Input.GetKeyDown("return"))
        {
            MoveAgent(action.NextAction("random", agent.coords));
        }
        else if (Input.GetKeyDown("space"))
        {
            MoveAgent(action.NextAction("prolog", agent.coords));
        }
        else if (Input.GetKeyDown("right"))
        {
            MoveAgent(new Coordinates(agent.coords.col + 1, agent.coords.row));
        }
        else if (Input.GetKeyDown("left"))
        {
            MoveAgent(new Coordinates(agent.coords.col - 1, agent.coords.row));
        }
        else if (Input.GetKeyDown("up"))
        {
            MoveAgent(new Coordinates(agent.coords.col, agent.coords.row + 1));
        }
        else if (Input.GetKeyDown("down"))
        {
            MoveAgent(new Coordinates(agent.coords.col, agent.coords.row - 1));
        }
        else if (Input.GetKeyDown("escape"))
        {
            gameOver = true;
        }
    }

    // Update is called once per frame
    void Update()
    {
        if (gameOver == true)
        {
            UnityEditor.EditorApplication.isPlaying = false;
            Application.Quit();
        }
        waitForAction();
    }
}