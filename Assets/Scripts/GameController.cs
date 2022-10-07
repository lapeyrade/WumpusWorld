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
                MoveCell(world.human);
                MoveCell(world.human2);
                SenseCell(world.human);
                SenseCell(world.human2);
                ActionCell(world.human);
                ActionCell(world.human2);
                prologInterface.PrintKB(world.human);
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
                    world.Move(agent, agent.MoveBack());
                    break;
                case "move_right":
                    world.Move(agent, new Coordinates(agent.coords.col + 1, agent.coords.row));
                    break;
                case "move_left":
                    world.Move(agent, new Coordinates(agent.coords.col - 1, agent.coords.row));
                    break;
                case "move_up":
                    world.Move(agent, new Coordinates(agent.coords.col, agent.coords.row + 1));
                    break;
                case "move_down":
                    world.Move(agent, new Coordinates(agent.coords.col, agent.coords.row - 1));
                    break;
                default:
                    break;
            }
        }

        else if (Input.GetKeyDown("right"))
            world.Move(agent, new Coordinates(agent.coords.col + 1, agent.coords.row));

        else if (Input.GetKeyDown("left"))
            world.Move(agent, new Coordinates(agent.coords.col - 1, agent.coords.row));

        else if (Input.GetKeyDown("up"))
            world.Move(agent, new Coordinates(agent.coords.col, agent.coords.row + 1));

        else if (Input.GetKeyDown("down"))
            world.Move(agent, new Coordinates(agent.coords.col, agent.coords.row - 1));
    }

    public void SenseCell(Human agent)
    {
        foreach (string element in world.map[agent.coords.col, agent.coords.row].Keys.ToList())
        {
            if (!world.agentMap[agent.coords.col, agent.coords.row].Keys.ToList().Contains(element))
                world.AddToGrids(agent.coords.col, agent.coords.row, element, true, false);

            prologInterface.AddCellContentKB(agent, new Coordinates(agent.coords.col, agent.coords.row), element);
        }

        List<string> cellContent = world.agentMap[agent.coords.col, agent.coords.row].Keys.ToList();

        if (!cellContent.Contains("wall"))
        {
            prologInterface.AddCellContentKB(agent, new Coordinates(agent.coords.col, agent.coords.row), "visited");

            makeInferences(agent);

            // if (cellContent.Contains("start") && (agent.nbGold == 1 || (agent.getAgentPersonalities().Contains("greedy") && agent.nbGold == world.nbGold)))
            if (cellContent.Contains("start") && (agent.nbGold == 1))
                SetGameOver("Game Won!", false);

            // else 
            if (cellContent.Contains("pit") || cellContent.Contains("wumpus"))
                SetGameOver("Game Lost!", false);
        }
    }

    public void makeInferences(Human agent)
    {
        AddAllElementToGrids("safe", true, false);
        AddAllElementToGrids("stenchyes", true, false);
        AddAllElementToGrids("wumpus", true, false);
        AddAllElementToGrids("breezeyes", true, false);
        AddAllElementToGrids("pit", true, false);
        AddAllElementToGrids("danger", true, false);
        AddAllElementToGrids("undefined", true, false);

        void AddAllElementToGrids(string element, bool updateMapAgent, bool updateMap)
        {
            foreach (Coordinates coords in prologInterface.CheckElement(element, agent))
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
                case "bump_wall":
                    prologInterface.RemoveCellContentKB(agent, agent.coords, "safe");
                    world.RemoveFromGrids(agent, agent.coords.col, agent.coords.row, agent.name, true, true);
                    agent.BumpWall();
                    world.AddToGrids(agent.coords.col, agent.coords.row, agent.name, true, true);
                    prologInterface.AddCellContentKB(agent, agent.coords, "human");
                    break;
                case "pickup_gold":
                    agent.TakeGold();
                    world.RemoveFromGrids(agent, agent.coords.col, agent.coords.row, "gold", true, true);
                    prologInterface.RemoveFromKB(agent, "nb_gold(_, _)");
                    prologInterface.AddToKB(agent, $"nb_gold({agent.name}, {agent.nbGold})", true);
                    break;
                case "shoot_right":
                    world.ShootArrow(agent, "right");
                    break;
                case "shoot_left":
                    world.ShootArrow(agent, "left");
                    break;
                case "shoot_up":
                    world.ShootArrow(agent, "up");
                    break;
                case "shoot_down":
                    world.ShootArrow(agent, "down");
                    break;
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