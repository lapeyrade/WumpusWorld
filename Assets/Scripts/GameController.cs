using UnityEngine;
using System.Linq;
using System.Collections.Generic;

/// <summary>
/// Manage the main dynamics of the game
/// </summary>
public class GameController : MonoBehaviour
{
    private bool gameOver = false;

    public GameObject gridManager;
    private World world;
    private PrologInterface prologInterface;

    private Agent agent;

    void Awake()
    {
        world = gridManager.GetComponent<World>();
        prologInterface = gridManager.GetComponent<PrologInterface>();
        agent = gridManager.GetComponent<Agent>();
    }

    void Update()
    {
        if (Input.anyKeyDown && !gameOver)
        {
            if (Input.GetKeyDown("escape"))
                SetGameOver("Exit Game!", true);
            else if (Input.GetKeyDown("return") || Input.GetKeyDown("space") || Input.GetKeyDown("right") ||
                        Input.GetKeyDown("left") || Input.GetKeyDown("up") || Input.GetKeyDown("down"))
            {
                MoveCell();
                SenseCell();
                ActionCell();
                prologInterface.PrintKB(agent);
            }
        }
    }

    public void MoveCell()
    {
        if (Input.GetKeyDown("space")) // Prolog Move
        {
            string nextMove = prologInterface.NextMove();
            switch (nextMove)
            {
                case "MoveBack":
                    agent.MoveBack();
                    break;
                case "MoveRight":
                    agent.Move(new Coordinates(agent.coords.col + 1, agent.coords.row));
                    break;
                case "MoveLeft":
                    agent.Move(new Coordinates(agent.coords.col - 1, agent.coords.row));
                    break;
                case "MoveUp":
                    agent.Move(new Coordinates(agent.coords.col, agent.coords.row + 1));
                    break;
                case "MoveDown":
                    agent.Move(new Coordinates(agent.coords.col, agent.coords.row - 1));
                    break;
                default:
                    break;
            }
        }

        else if (Input.GetKeyDown("return")) // Random Move
        {
            string randomMove = prologInterface.RandomMove();
            switch (randomMove)
            {
                case "MoveBack":
                    agent.MoveBack();
                    break;
                case "MoveRight":
                    agent.Move(new Coordinates(agent.coords.col + 1, agent.coords.row));
                    break;
                case "MoveLeft":
                    agent.Move(new Coordinates(agent.coords.col - 1, agent.coords.row));
                    break;
                case "MoveUp":
                    agent.Move(new Coordinates(agent.coords.col, agent.coords.row + 1));
                    break;
                case "MoveDown":
                    agent.Move(new Coordinates(agent.coords.col, agent.coords.row - 1));
                    break;
                default:
                    break;
            }
        }

        else if (Input.GetKeyDown("right"))
            agent.Move(new Coordinates(agent.coords.col + 1, agent.coords.row));

        else if (Input.GetKeyDown("left"))
            agent.Move(new Coordinates(agent.coords.col - 1, agent.coords.row));

        else if (Input.GetKeyDown("up"))
            agent.Move(new Coordinates(agent.coords.col, agent.coords.row + 1));

        else if (Input.GetKeyDown("down"))
            agent.Move(new Coordinates(agent.coords.col, agent.coords.row - 1));
    }

    public void SenseCell()
    {
        foreach (string element in agent.map[agent.coords.col, agent.coords.row].Keys.ToList())
        {
            prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row), element);
        }

        foreach (string element in world.map[agent.coords.col, agent.coords.row].Keys.ToList())
        {
            if (!agent.map[agent.coords.col, agent.coords.row].Keys.ToList().Contains(element))
                world.AddToGrids(agent.coords.col, agent.coords.row, element, true, false);

            prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row), element);
        }

        List<string> cellContent = agent.map[agent.coords.col, agent.coords.row].Keys.ToList();

        if (!cellContent.Contains("stenchyes"))
            prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row), "stenchno");

        if (!cellContent.Contains("wumpusyes"))
            prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row), "wumpusno");

        if (!cellContent.Contains("wumpusyes") && !cellContent.Contains("stenchyes") &&
            !cellContent.Contains("breeze") && !cellContent.Contains("pit"))
            prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row), "safe");

        if (cellContent.Contains("emptyCell") && cellContent.Count > 1)
            world.RemoveFromGrids(agent.coords.col, agent.coords.row, "emptyCell", true, false);

        if (cellContent.Contains("start") && agent.nbGold == world.nbGold)
            SetGameOver("Game Won!", false);

        else if (cellContent.Contains("pit") || cellContent.Contains("wumpus"))
            SetGameOver("Game Lost!", false);
    }

    public void ActionCell()
    {
        bool ActionLeftToDo = true;
        while (ActionLeftToDo)
        {
            string action = prologInterface.NextAction();
            switch (action)
            {
                case "HitWall":
                    prologInterface.RemoveCellContentKB(agent.coords, "safe");
                    agent.HitWall();
                    prologInterface.AddCellContentKB(agent.coords, "agent");
                    break;
                case "TakeGold":
                    agent.TakeGold();
                    prologInterface.RemoveFromKB("nb_gold_agent(_)");
                    prologInterface.AddToKB($"nb_gold_agent({agent.nbGold})");
                    break;
                case "MarkNearCellSafe":
                    agent.MarkNearCellSafe();
                    prologInterface.AddCellContentKB(new Coordinates(agent.coords.col + 1, agent.coords.row), "safe");
                    prologInterface.AddCellContentKB(new Coordinates(agent.coords.col - 1, agent.coords.row), "safe");
                    prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row + 1), "safe");
                    prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row - 1), "safe");
                    break;
                case "CheckForWumpus":
                    for (int row = agent.coords.row - 3; row < agent.coords.row + 3; row++)
                    {
                        for (int col = agent.coords.col - 3; col < agent.coords.col + 3; col++)
                        {
                            Debug.Log(col + " " + row + " " + prologInterface.CheckCellElement(new Coordinates(col, row), "wumpusyes"));
                            // Debug.Log(col + " " + row + " " + prologInterface.CheckCellElement(new Coordinates(col, row), "wumpus"));
                        }
                    }
                    break;
                case "CheckForPit":
                    agent.CheckForPit();
                    break;
                case "MoveNextCell":
                    ActionLeftToDo = false;
                    break;
                default:
                    break;
            }
        }
    }

    public void SetGameOver(string message, bool exitApp)
    {
        gameOver = true;
        Debug.Log(message);

        if (exitApp)
        {
            Application.Quit();
            UnityEditor.EditorApplication.isPlaying = false;
        }
    }
}