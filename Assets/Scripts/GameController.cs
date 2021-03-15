using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;

public class GameController : MonoBehaviour
{
    public int randomSeed = 4;
    public int gridSizeX = 5;
    public int gridSizeY = 5;
    public int startX = 1;
    public int startY = 1;
    public float tileSize = 1.05f;
    public int pits = 3;
    public int wumpuses = 1;
    public int golds = 1;
    public int agents = 1;

    public Coordinates grid;
    public Coordinates startCoords;
    public Dictionary<string, GameObject>[,] map;
    public Dictionary<string, GameObject>[,] mapFull;

    private bool gameOver = false;

    private Action action;
    private Agent agent;

    void Awake()
    {
        Random.InitState(randomSeed); // Random Seed

        grid = new Coordinates(gridSizeX + 2, gridSizeY + 2);
        startCoords = new Coordinates(startX, startY);
        action = new Action(wumpuses, golds, startCoords);

        map = new Dictionary<string, GameObject>[grid.x, grid.y];
        mapFull = new Dictionary<string, GameObject>[grid.x, grid.y];

        if ((pits + wumpuses + golds + agents) > (grid.x * grid.y))
        {
            Debug.LogError("Map too small.");
            gameOver = true;
        }
        else
        {
            GenerateGrid();
            GrenerateWall();
            GeneratePit();
            GenerateWumpus();
            GenerateGold();
            GenerateAgent();
        }
    }

    private void GenerateGrid()
    {
        for (int col = 0; col < grid.x; col++)
        {
            for (int row = 0; row < grid.y ; row++) 
            {
                map[col, row] = new Dictionary<string, GameObject>();
                mapFull[col, row] = new Dictionary<string, GameObject>();
                if(!(col == startCoords.x && row == startCoords.y))
                    AddToGrids(col, row, "emptyCell", true, true);
            }
        }
        AddToGrids(startCoords.x, startCoords.y, "start", true, true);
    }

    private void GrenerateWall()
    {
        for (int row = 0; row < grid.y; row++) // right
        {
            RemoveFromGrids(grid.x - 1, row, "emptyCell", false, true);
            RemoveFromGrids(grid.x - 1, row, "safe", false, true);
            AddToGrids(grid.x - 1, row, "wall", false, true); 
        }
        for (int row = 0; row < grid.y; row++) // left
        {
            RemoveFromGrids(0, row, "emptyCell", false, true);
            RemoveFromGrids(0, row, "safe", false, true);
            AddToGrids(0, row, "wall", false, true);
        }
        for (int col = 0; col < grid.x; col++) // top
        {
            RemoveFromGrids(col, grid.y - 1, "emptyCell", false, true);
            RemoveFromGrids(col, grid.y - 1, "safe", false, true);
            AddToGrids(col, grid.y - 1, "wall", false, true); 
        }
        for (int col = 0; col < grid.x; col++) // bottom
        {
            RemoveFromGrids(col, 0, "emptyCell", false, true);
            RemoveFromGrids(col, 0, "safe", false, true);
            AddToGrids(col, 0, "wall", false, true); 
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
            if (mapFull[col, row].ContainsKey(keyGenerated) == false && !mapFull[col, row].ContainsKey("pit") && !mapFull[col, row].ContainsKey("wall"))
            {
                AddToGrids(col, row, keyGenerated, false, true);
            }
        }
    }

    private void GeneratePit()
    {

        int col = 0;
        int row = 0;
        for (int pit = 0; pit < pits; pit++)
        {
            while (mapFull[col, row].ContainsKey("start") || mapFull[col, row].ContainsKey("pit") || mapFull[col, row].ContainsKey("wumpus") || mapFull[col, row].ContainsKey("gold") || mapFull[col, row].ContainsKey("wall") || mapFull[col, row].ContainsKey("agent") || (col == startX && row == startY + 1) || (col == startX + 1 && row == startY))
            {
                col = Random.Range(0, grid.x - 1);
                row = Random.Range(0, grid.y - 1);
            }
            AddToGrids(col, row, "pit", false, true);
            RemoveFromGrids(col, row, "emptyCell", false, true);
            GenerateAroundCell(col, row, "breeze");  // breeze Generation
        }
    }

    private void GenerateWumpus()
    {
        int col = 0;
        int row = 0;
        for (int wumpus = 0; wumpus < wumpuses; wumpus++)
        {
            while (mapFull[col, row].ContainsKey("start") || mapFull[col, row].ContainsKey("pit") || mapFull[col, row].ContainsKey("wumpus") || mapFull[col, row].ContainsKey("gold") || mapFull[col, row].ContainsKey("wall") || mapFull[col, row].ContainsKey("agent") || (col == startX && row == startY + 1) || (col == startX + 1 && row == startY))
            {
                col = Random.Range(0, grid.x - 1);
                row = Random.Range(0, grid.y - 1);
            }
            AddToGrids(col, row, "wumpus", false, true);
            // RemoveFromGrids(col, row, "emptyCell", true, true);
            GenerateAroundCell(col, row, "stench");  // Stench Generation
        }
    }

    private void GenerateGold()
    {
        int col = 0;
        int row = 0;
        for (int gold = 0; gold < golds; gold++)
        {
            while (mapFull[col, row].ContainsKey("start") || mapFull[col, row].ContainsKey("pit") || mapFull[col, row].ContainsKey("wumpus") || mapFull[col, row].ContainsKey("gold") || mapFull[col, row].ContainsKey("agent") ||mapFull[col, row].ContainsKey("wall"))
            {
                col = Random.Range(0, grid.x - 1);
                row = Random.Range(0, grid.y - 1);
            }
            AddToGrids(col, row, "gold", false, true);
        }
    }

    private void GenerateAgent()
    {
        if (agents == 1)
        {
            AddToGrids(startCoords.x, startCoords.y, "agent", true, true);
            agent = new Agent(startCoords, wumpuses);
            CheckNearCells(startCoords);
        }
        else if (agents > 1)
        {
            int col = startCoords.x;
            int row = startCoords.y;
            for (int agent = 0; agent < agents; agent++)
            {
                while (mapFull[col, row].ContainsKey("start") || mapFull[col, row].ContainsKey("pit") || mapFull[col, row].ContainsKey("wumpus") || mapFull[col, row].ContainsKey("gold") || mapFull[col, row].ContainsKey("agent"))
                {
                    col = Random.Range(0, grid.x);
                    row = Random.Range(0, grid.y);
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
            map[col, row][cell].transform.position = new Vector2((col - grid.x / 2 - 1f) * tileSize, row * tileSize);
        }

        if(updateMapFull && mapFull[col, row].ContainsKey(cell) == false)
        {
            mapFull[col, row].Add(cell, (GameObject)Instantiate(Resources.Load(cell), transform));
            mapFull[col, row][cell].transform.position = new Vector2((col + grid.x /2 + 1f) * tileSize, row * tileSize);
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
        if(!map[coords.x + 1, coords.y].ContainsKey("visited")) // right
        {
            foreach (string state in action.CheckCell(new Coordinates(coords.x + 1, coords.y)))
            {
                AddToGrids(coords.x + 1, coords.y, state, true, false);
                RemoveFromGrids(coords.x + 1, coords.y, "emptyCell", true, false);
            }
        }
        if(!map[coords.x - 1, coords.y].ContainsKey("visited")) // left
        {
            foreach (string state in action.CheckCell(new Coordinates(coords.x - 1, coords.y)))
            {
                AddToGrids(coords.x - 1, coords.y, state, true, false);
                RemoveFromGrids(coords.x - 1, coords.y, "emptyCell", true, false);
            }
        }
        if(!map[coords.x, coords.y + 1].ContainsKey("visited")) // top
        {
            foreach (string state in action.CheckCell(new Coordinates(coords.x, coords.y + 1)))
            {
                AddToGrids(coords.x, coords.y + 1, state, true, false);
                RemoveFromGrids(coords.x, coords.y + 1, "emptyCell", true, false);
            }
        }
        if(!map[coords.x, coords.y - 1].ContainsKey("visited")) // bottom
        {
            foreach (string state in action.CheckCell(new Coordinates(coords.x, coords.y - 1)))
            {
                AddToGrids(coords.x, coords.y - 1, state, true, false);
                RemoveFromGrids(coords.x, coords.y -1, "emptyCell", true, false);
            }
        }
    }


    private void MoveAgent(Coordinates newCoords)
    {
        if (!mapFull[newCoords.x, newCoords.y].ContainsKey("wall"))
        {
            action.RemoveFromKB($"cell({agent.coords.x}, {agent.coords.y}, agent)");
            action.AddFactKB($"cell({newCoords.x}, {newCoords.y}, agent)");
            action.RemoveFromKB($"cell({newCoords.x}, {newCoords.y}, safe)");
            action.AddFactKB($"cell({newCoords.x}, {newCoords.y}, visited)");

            RemoveFromGrids(newCoords.x, newCoords.y, "safe", true, true);
            RemoveFromGrids(newCoords.x, newCoords.y, "emptyCell", true, true);

            if(!map[newCoords.x, newCoords.y].ContainsKey("start"))
                AddToGrids(newCoords.x, newCoords.y, "visited", true, true);

            if (newCoords.x == agent.prevCoords.Peek().x && newCoords.y == agent.prevCoords.Peek().y && agent.prevCoords.Count() > 1)
                agent.prevCoords.Pop();
            else
                agent.prevCoords.Push(new Coordinates(agent.coords.x, agent.coords.y));

            RemoveFromGrids(agent.coords.x, agent.coords.y, "agent", true, true);
            AddToGrids(newCoords.x, newCoords.y, "agent", true, true);

            if(agent.coords.x < newCoords.x)
            {
                map[newCoords.x, newCoords.y]["agent"].GetComponent<SpriteRenderer>().flipX = false;
                mapFull[newCoords.x, newCoords.y]["agent"].GetComponent<SpriteRenderer>().flipX = false;
            }
            else if(agent.coords.x > newCoords.x)
            {
                map[newCoords.x, newCoords.y]["agent"].GetComponent<SpriteRenderer>().flipX = true;
                mapFull[newCoords.x, newCoords.y]["agent"].GetComponent<SpriteRenderer>().flipX = true;
            }

            agent.coords.x = newCoords.x;
            agent.coords.y = newCoords.y;

            ActionCase(mapFull[agent.coords.x, agent.coords.y]);
        }
        else
        {
            RemoveFromGrids(newCoords.x, newCoords.y, "emptyCell", true, false);
            RemoveFromGrids(newCoords.x, newCoords.y, "safe", true, false);
            AddToGrids(newCoords.x, newCoords.y, "wall", true, false);
            action.RemoveFromKB($"cell({newCoords.x}, {newCoords.y}, safe)");
            action.AddFactKB($"cell({newCoords.x}, {newCoords.y}, wall)");
            action.printKB();
        }
    }

    private void ActionCase(Dictionary<string, GameObject> position)
    {
        Coordinates coordsWumpus = action.WumpusFound();
        if (coordsWumpus != null && !map[coordsWumpus.x, coordsWumpus.y].ContainsKey("wumpusDead"))
        {
            RemoveFromGrids(coordsWumpus.x, coordsWumpus.y, "wumpus", true, true);
            AddToGrids(coordsWumpus.x, coordsWumpus.y, "safe", true, false);
            AddToGrids(coordsWumpus.x, coordsWumpus.y, "wumpusDead", true, true);

            action.RemoveFromKB($"cell({coordsWumpus.x}, {coordsWumpus.y}, wumpus)");
            action.AddFactKB($"cell({coordsWumpus.x}, {coordsWumpus.y}, safe)");
            action.AddFactKB($"cell({coordsWumpus.x}, {coordsWumpus.y}, wumpusDead)");
            mapFull[coordsWumpus.x, coordsWumpus.y].Remove("wumpus");
            agent.arrows -= 1;
        }

        if (position.ContainsKey("pit") || position.ContainsKey("wumpus"))
            gameOver = true;

        if (position.ContainsKey("gold"))
        {
            RemoveFromGrids(agent.coords.x, agent.coords.y, "gold", false, true);

            action.AddFactKB($"cell({agent.coords.x}, {agent.coords.y}, gold)");
            action.RemoveFromKB($"goldAgent({agent.golds})");
            agent.golds += 1;
            action.AddFactKB($"goldAgent({agent.golds})");
            action.RemoveFromKB($"cell({agent.coords.x}, {agent.coords.y}, gold)");
            if (agent.golds == golds)
            {
                action.RemoveFromKB($"allGoldsFound(false)");
                action.AddFactKB($"allGoldsFound(true)");
            }
        }
        if (position.ContainsKey("breeze"))
        {
            AddToGrids(agent.coords.x, agent.coords.y, "breeze", true, false);
            action.RemoveFromKB($"cell({agent.coords.x}, {agent.coords.y}, safe)");
            action.AddFactKB($"cell({agent.coords.x}, {agent.coords.y}, breeze)");
        }
        if (position.ContainsKey("stench"))
        {
            AddToGrids(agent.coords.x, agent.coords.y, "stench", true, false);
            action.RemoveFromKB($"cell({agent.coords.x}, {agent.coords.y}, safe)");
            action.AddFactKB($"cell({agent.coords.x}, {agent.coords.y}, stench)");
        }
        if (position.ContainsKey("start"))
        {
            if (agent.golds == golds)
            {
                gameOver = true;
            }
        }
        if (!position.ContainsKey("breeze") && !position.ContainsKey("stench"))
        {
            action.AddFactKB($"cell({agent.coords.x}, {agent.coords.y}, safe)");
        }

        action.CheckNearCells(new Coordinates(agent.coords.x, agent.coords.y));
        CheckNearCells(new Coordinates(agent.coords.x, agent.coords.y));

        action.printKB();
    }

    private void waitForAction()
    {
        if (Input.GetKeyDown("return"))
        {
            MoveAgent(action.NextAction("random", agent.coords, agent.prevCoords.Peek()));
        }
        else if (Input.GetKeyDown("space"))
        {
            MoveAgent(action.NextAction("prolog", agent.coords, agent.prevCoords.Peek()));
        }
        else if (Input.GetKeyDown("right"))
        {
            MoveAgent(new Coordinates(agent.coords.x + 1, agent.coords.y));
        }
        else if (Input.GetKeyDown("left"))
        {
            MoveAgent(new Coordinates(agent.coords.x - 1, agent.coords.y));
        }
        else if (Input.GetKeyDown("up"))
        {
            MoveAgent(new Coordinates(agent.coords.x, agent.coords.y + 1));
        }
        else if (Input.GetKeyDown("down"))
        {
            MoveAgent(new Coordinates(agent.coords.x, agent.coords.y - 1));
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