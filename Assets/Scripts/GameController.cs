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
    private bool autoMode = true;

    [SerializeField]
    private float timerInterval = 0.01f;

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

    public void MoveCell()
    {
        if (Input.GetKeyDown("space") || autoMode || Input.GetKeyDown("return"))
        {
            string move = "";

            if (Input.GetKeyDown("space") || autoMode) // Prolog Move
            {
                move = prologInterface.NextMove();
                Debug.Log(move);
            }
            else if (Input.GetKeyDown("return")) // Random Move
            {
                move = prologInterface.RandomMove();
                Debug.Log(move);
            }

            switch (move)
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

            makeInferences();

            if (cellContent.Contains("start") && agent.nbGold == world.nbGold)
                SetGameOver("Game Won!", false);

            else if (cellContent.Contains("pit") || cellContent.Contains("wumpus"))
                SetGameOver("Game Lost!", false);
        }
    }

    public void makeInferences()
    {
        AddAllElementToGrids("safe", true, false);
        AddAllElementToGrids("stenchyes", true, false);
        AddAllElementToGrids("wumpus", true, false);
        AddAllElementToGrids("breezeyes", true, false);
        AddAllElementToGrids("pit", true, false);

        void AddAllElementToGrids(string element, bool updateMapAgent, bool updateMap)
        {
            foreach (Coordinates coords in prologInterface.CheckElement(element))
            {
                world.AddToGrids(coords.col, coords.row, element, true, false);
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
                    break;
                case "ShotLeft":
                    world.ShotArrow("left");
                    break;
                case "ShotUp":
                    world.ShotArrow("up");
                    break;
                case "ShotDown":
                    world.ShotArrow("down");
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