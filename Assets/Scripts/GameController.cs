using UnityEngine;
using System.Linq;
using System.Collections;
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

    [SerializeField]
    private bool autoMode = false;

    [SerializeField]
    private float timerInterval = 1;

    private float timer = 0;

    private bool turnFinished = true;

    void Awake()
    {
        world = gridManager.GetComponent<World>();
        prologInterface = gridManager.GetComponent<PrologInterface>();
        agent = gridManager.GetComponent<Agent>();
    }

    void Update()
    {
        if (autoMode && timer < timerInterval)
            timer += Time.deltaTime;
        else if (turnFinished)
        {
            turnFinished = false;
            PlayTurn();
            timer = 0;
            turnFinished = true;
        }
    }

    private void PlayTurn()
    {
        if ((Input.anyKeyDown || autoMode) && !gameOver)
        {
            if (Input.GetKeyDown("escape"))
                SetGameOver("Exit Game!", true);
            else if (Input.GetKeyDown("return") || Input.GetKeyDown("space") || Input.GetKeyDown("right") ||
                        Input.GetKeyDown("left") || Input.GetKeyDown("up") || Input.GetKeyDown("down") || autoMode)
            {
                MoveCell();
                SenseCell();
                ActionCell();
                prologInterface.PrintKB(agent);
            }
        }
    }
    IEnumerator ExampleCoroutine()
    {
        //Print the time of when the function is first called.
        Debug.Log("Started Coroutine at timestamp : " + Time.time);

        //yield on a new YieldInstruction that waits for 5 seconds.
        yield return new WaitForSeconds(1);
        PlayTurn();
        //After we have waited 5 seconds print the time again.
        Debug.Log("Finished Coroutine at timestamp : " + Time.time);
    }

    public void MoveCell()
    {
        if (Input.GetKeyDown("space") || autoMode) // Prolog Move
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
        foreach (string element in world.map[agent.coords.col, agent.coords.row].Keys.ToList())
        {
            if (!agent.map[agent.coords.col, agent.coords.row].Keys.ToList().Contains(element))
                world.AddToGrids(agent.coords.col, agent.coords.row, element, true, false);

            prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row), element);
        }

        List<string> cellContent = agent.map[agent.coords.col, agent.coords.row].Keys.ToList();

        if (!cellContent.Contains("wall"))
        {
            prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row), "visited");

            if (!cellContent.Contains("stenchyes"))
                prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row), "stenchno");

            if (!cellContent.Contains("wumpusyes"))
                prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row), "wumpusno");

            if (!cellContent.Contains("breezeyes"))
                prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row), "breezeno");

            if (!cellContent.Contains("pityes"))
                prologInterface.AddCellContentKB(new Coordinates(agent.coords.col, agent.coords.row), "pitno");

            makeInferences();

            if (cellContent.Contains("emptyCell") && cellContent.Count > 1)
                world.RemoveFromGrids(agent.coords.col, agent.coords.row, "emptyCell", true, false);

            if (cellContent.Contains("start") && agent.nbGold == world.nbGold)
                SetGameOver("Game Won!", false);

            else if (cellContent.Contains("pit") || cellContent.Contains("wumpus"))
                SetGameOver("Game Lost!", false);
        }
    }

    void makeInferences()
    {
        for (int col = world.gridMin.col; col < world.gridMax.col; col++)
        {
            for (int row = world.gridMin.row; row < world.gridMax.row; row++)
            {
                if (prologInterface.CheckCellElement(new Coordinates(col, row), "safe"))
                    world.AddToGrids(col, row, "safe", true, false);
                if (!prologInterface.CheckCellElementFalse(new Coordinates(col, row), "stench"))
                    world.AddToGrids(col, row, "stenchyes", true, false);
                if (!prologInterface.CheckCellElementFalse(new Coordinates(col, row), "breeze"))
                    world.AddToGrids(col, row, "breezeyes", true, false);
                if (!prologInterface.CheckCellElementFalse(new Coordinates(col, row), "wumpus"))
                    world.AddToGrids(col, row, "wumpus", true, false);
                if (!prologInterface.CheckCellElementFalse(new Coordinates(col, row), "pit"))
                    world.AddToGrids(col, row, "pit", true, false);
            }
        }
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
                case "ShotRight":
                    world.ShotArrow("right");
                    ActionLeftToDo = false;
                    break;
                case "ShotLeft":
                    world.ShotArrow("left");
                    ActionLeftToDo = false;
                    break;
                case "ShotUp":
                    world.ShotArrow("up");
                    ActionLeftToDo = false;
                    break;
                case "ShotDown":
                    world.ShotArrow("down");
                    ActionLeftToDo = false;
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