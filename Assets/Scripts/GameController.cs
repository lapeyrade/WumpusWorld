using System.Linq;
using UnityEngine;

/// <summary>
/// Manage the main dynamics of the game
/// </summary>
public class GameController : MonoBehaviour
{
    public GameObject gridManager;
    private World world;
    private PrologInterface prologInterface;

    [SerializeField] public bool autoMode = true;
    [SerializeField] public float timerInterval = 0.01f;
    private float timer = 0.0f;
    private bool gameOver = false;

    protected void Awake()
    {
        world = gridManager.GetComponent<World>();
        prologInterface = gridManager.GetComponent<PrologInterface>();
    }

    protected void Update()
    {
        if (autoMode && timer < timerInterval)
            timer += Time.deltaTime;
        else
        {
            PlayTurn();
            timer = 0.0f;
        }
    }

    public void PlayTurn(bool firstTurn = false)
    {
        if ((Input.anyKeyDown || autoMode || firstTurn) && !gameOver)
        {
            if (Input.GetKeyDown("escape"))
                SetGameOver("Exit Game!", true);

            else if (Input.GetKeyDown("return") || Input.GetKeyDown("space") || Input.GetKeyDown("right") ||
                Input.GetKeyDown("left") || Input.GetKeyDown("up") || Input.GetKeyDown("down") || autoMode || firstTurn)
            {
                foreach (Human agent in world.agents)
                {
                    if (firstTurn)
                        agent.personalities = prologInterface.GetPersonalities(agent);

                    MoveCell(agent, firstTurn);
                    SenseCell(agent);
                    ActionCell(agent);
                }

                prologInterface.PrintKB(world.agents);
            }
        }
    }

    public void MoveCell(Human agent, bool firstTurn = false)
    {
        if (firstTurn)
            world.Move(agent, agent.startCoord);

        string move = "";

        if (Input.GetKeyDown("right"))
            move = "move_right";
        else if (Input.GetKeyDown("left"))
            move = "move_left";
        else if (Input.GetKeyDown("up"))
            move = "move_up";
        else if (Input.GetKeyDown("down"))
            move = "move_down";
        else if (Input.GetKeyDown("space") || autoMode) // Prolog Move
            move = prologInterface.NextMove(agent);
        else if (Input.GetKeyDown("return")) // Random Move
            move = prologInterface.RandomMove(agent);

        if (!firstTurn)
            Debug.Log(move);

        switch (move)
        {
            case "move_back":
                world.Move(agent, agent.MoveBack());
                break;
            case "move_right":
                world.Move(agent, new Vector2Int(agent.coord.x + 1, agent.coord.y));
                break;
            case "move_left":
                world.Move(agent, new Vector2Int(agent.coord.x - 1, agent.coord.y));
                break;
            case "move_up":
                world.Move(agent, new Vector2Int(agent.coord.x, agent.coord.y + 1));
                break;
            case "move_down":
                world.Move(agent, new Vector2Int(agent.coord.x, agent.coord.y - 1));
                break;
            default:
                break;
        }
    }

    public void SenseCell(Human agent)
    {
        if (agent.startCoord == agent.coord && agent.nbGold == 1)
            SetGameOver("Game Won!", false);

        foreach (string element in world.map[agent.coord.x, agent.coord.y].Except(world.agentMap[agent.coord.x, agent.coord.y]).Select(x => x.Item1))
        {
            world.AddToGrids(agent.coord, element, true, false);
            prologInterface.AddCellContentKB(agent.coord, element);
        }

        if (!world.agentMap[agent.coord.x, agent.coord.y].Exists(x => x.Item1 is "wall" or "pit" or "wumpus"))
        {
            prologInterface.AddCellContentKB(agent.coord, "visited");
            MakeInferences();
        }
        else if (world.agentMap[agent.coord.x, agent.coord.y].Exists(x => x.Item1 is "wumpus" or "pit"))
        {
            world.AddToGrids(agent.coord, "danger", true, false);
            SetGameOver("Game Lost!", false);
        }
    }

    public void MakeInferences()
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
            foreach (Vector2Int coord in prologInterface.CheckElement(element))
            {
                if (coord.x >= world.gridMin.x && coord.x < world.gridMax.x && coord.y >= world.gridMin.y && coord.y < world.gridMax.y)
                    world.AddToGrids(coord, element, updateMapAgent, updateMap);
            }
        }
    }

    public void ActionCell(Human agent)
    {
        bool ActionLeftToDo = !gameOver;
        while (ActionLeftToDo)
        {
            string action = prologInterface.NextAction(agent);
            if (action != "null")
                Debug.Log(action);

            switch (action)
            {
                case "bump_wall":
                    prologInterface.RemoveCellContentKB(agent.coord, "safe");
                    world.Move(agent, agent.MoveBack());
                    break;
                case "pickup_gold":
                    agent.nbGold += 1;
                    world.RemoveFromGrids(agent.coord, "gold", true, true);
                    prologInterface.RemoveFromKB($"nb_gold({agent.agentName}, _)");
                    prologInterface.AddToKB($"nb_gold({agent.agentName}, {agent.nbGold})", true);
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