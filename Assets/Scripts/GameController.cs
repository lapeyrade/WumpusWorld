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

    private Human human;
    // private Human human2;
    private Dog dog;

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
        human = gridManager.GetComponent<Human>();
        // human2 = gridManager.GetComponent<Human>();
        dog = gridManager.GetComponent<Dog>();
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
                MoveCell(human);
                // MoveCell(human2);
                SenseCell(human);
                // SenseCell(human2);
                ActionCell(human);
                // ActionCell(human2);
                prologInterface.PrintKB(human);
            }
        }
    }

    public void MoveCell(Human agent)
    {
        if (Input.GetKeyDown("space") || autoMode || Input.GetKeyDown("return"))
        {
            string move = "";

            if (Input.GetKeyDown("space") || autoMode) // Prolog Move
            {
                move = prologInterface.NextMove(agent);
                Debug.Log(move);
            }
            else if (Input.GetKeyDown("return")) // Random Move
            {
                move = prologInterface.RandomMove(agent);
                Debug.Log(move);
            }

            switch (move)
            {
                case "move_back":
                    agent.MoveBack();
                    break;
                case "move_right":
                    agent.Move(new Coordinates(agent.coords.col + 1, agent.coords.row));
                    break;
                case "move_left":
                    agent.Move(new Coordinates(agent.coords.col - 1, agent.coords.row));
                    break;
                case "move_up":
                    agent.Move(new Coordinates(agent.coords.col, agent.coords.row + 1));
                    break;
                case "move_down":
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

    public void SenseCell(Human agent)
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

            if (cellContent.Contains("start") && (agent.nbGold == 1 || (agent.getAgentPersonalities().Contains("greedy") && agent.nbGold == world.nbGold)))
                SetGameOver("Game Won!", false);

            // else 
            if (cellContent.Contains("pit") || cellContent.Contains("wumpus"))
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
        AddAllElementToGrids("danger", true, false);
        AddAllElementToGrids("unknow", true, false);

        void AddAllElementToGrids(string element, bool updateMapAgent, bool updateMap)
        {
            foreach (Coordinates coords in prologInterface.CheckElement(element))
            {
                if (coords.col >= world.gridMin.col && coords.col < world.gridMax.col && coords.row >= world.gridMin.row && coords.row < world.gridMax.row)
                    world.AddToGrids(coords.col, coords.row, element, updateMapAgent, updateMap);
            }
        }
    }

    public void ActionCell(Human agent)
    {
        bool ActionLeftToDo = true;
        while (ActionLeftToDo)
        {
            string action = prologInterface.NextAction(agent);
            switch (action)
            {
                case "hit_wall":
                    prologInterface.RemoveCellContentKB(agent.coords, "safe");
                    agent.HitWall();
                    prologInterface.AddCellContentKB(agent.coords, "human");
                    break;
                case "take_gold":
                    human.TakeGold();
                    prologInterface.RemoveFromKB("nb_gold_agent(_)");
                    prologInterface.AddToKB($"nb_gold_agent({human.nbGold})", true);
                    break;
                case "shoot_right":
                    world.ShootArrow("right");
                    break;
                case "shoot_left":
                    world.ShootArrow("left");
                    break;
                case "shoot_up":
                    world.ShootArrow("up");
                    break;
                case "shoot_down":
                    world.ShootArrow("down");
                    break;
                // case "move_next_cell":
                // break;
                default:
                    ActionLeftToDo = false;
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