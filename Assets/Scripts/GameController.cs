using System;
using System.Linq;
using UnityEngine;

/// <summary>
/// Manage the main dynamics of the game
/// </summary>
public class GameController : MonoBehaviour
{
    public GameObject gridManager;
    private World _world;
    private PrologInterface _prologInterface;

    [SerializeField] public bool autoMode = true;
    [SerializeField] public bool consoleLog;
    [Range(0.0f, 1.0f)][SerializeField] public float timerInterval = 0.016f;
    private float _timer;
    // private float _timer2;
    private bool _gameOver;
    
    protected void Awake()
    {
        _world = gridManager.GetComponent<World>();
        _prologInterface = gridManager.GetComponent<PrologInterface>();
    }

    protected void Update()
    {
        if (autoMode && _timer < timerInterval)
        {
            _timer += Time.deltaTime;
            // _timer2 += Time.deltaTime;
        }
        else
        {
            PlayTurn();
            _timer = 0.0f;
        }
    }

    public void PlayTurn(bool firstTurn = false)
    {
        if ((Input.anyKeyDown || autoMode || firstTurn) && !_gameOver)
        {
            if (Input.GetKeyDown("escape"))
                SetGameOver("Exit Game!", true);
            else if (Input.GetKeyDown("backspace"))
            {
                ScreenCapture.CaptureScreenshot("Screenshots/screenshot " + System.DateTime.Now.ToString("MM-dd-yy (HH-mm-ss)") + ".png");
                Debug.Log("Screenshot saved!");
            }
            else if (Input.GetKeyDown("return") || Input.GetKeyDown("space") || Input.GetKeyDown("right") ||
                     Input.GetKeyDown("left") || Input.GetKeyDown("up") || Input.GetKeyDown("down") || autoMode || firstTurn)
            {
                foreach (Human agent in _world.agents)
                {
                    if (firstTurn)
                        agent.personalities = _prologInterface.GetPersonalities(agent);

                    MoveCell(agent, firstTurn);
                    SenseCell(agent);
                    ActionCell(agent);
                }

                _prologInterface.PrintKb(_world.agents);
            }
        }
    }

    private void MoveCell(Human agent, bool firstTurn = false)
    {
        if (firstTurn)
            _world.Move(agent, agent.startCoord);

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
            move = _prologInterface.NextMove(agent);
        else if (Input.GetKeyDown("return")) // Random Move
            move = _prologInterface.RandomMove(agent);

        if (!firstTurn && consoleLog)
            Debug.Log($"{agent.id}: {move}");

        switch (move)
        {
            case "move_back":
                _world.Move(agent, agent.MoveBack());
                break;
            case "move_right":
                _world.Move(agent, new Vector2Int(agent.coord.x + 1, agent.coord.y));
                break;
            case "move_left":
                _world.Move(agent, new Vector2Int(agent.coord.x - 1, agent.coord.y));
                break;
            case "move_up":
                _world.Move(agent, new Vector2Int(agent.coord.x, agent.coord.y + 1));
                break;
            case "move_down":
                _world.Move(agent, new Vector2Int(agent.coord.x, agent.coord.y - 1));
                break;
        }
    }

    private void SenseCell(Human agent)
    {
        if (agent.startCoord == agent.coord && agent.nbGold == 1)
            SetGameOver($"{agent.id} Won!", false);

        foreach (string element in _world.Map[agent.coord.x, agent.coord.y]
                     .Except(_world.AgentMap[agent.coord.x, agent.coord.y]).Select(x => x.Item1))
        {
            _world.AddToGrids(agent.coord, element, true, false);
            _prologInterface.AddCellContentKb(element, agent.coord);
        }

        if (!_world.AgentMap[agent.coord.x, agent.coord.y].Exists(x => x.Item1 is "wall" or "pit" or "wumpus"))
        {
            _prologInterface.AddCellContentKb("visited", agent.coord);
            MakeInferences();
        }
        else if (_world.AgentMap[agent.coord.x, agent.coord.y].Exists(x => x.Item1 is "wumpus" or "pit"))
        {
            _world.AddToGrids(agent.coord, "danger", true, false);
            SetGameOver($"{agent.id} Lost!", false);
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
            foreach (Vector2Int coord in _prologInterface.CheckElement(element))
            {
                if (coord.x >= _world.gridMin.x && coord.x < _world.gridMax.x && coord.y >= _world.gridMin.y && coord.y < _world.gridMax.y)
                    _world.AddToGrids(coord, element, updateMapAgent, updateMap);
            }
        }
    }

    private void ActionCell(Human agent)
    {
        bool actionLeftToDo = !_gameOver;
        while (actionLeftToDo)
        {
            string action = _prologInterface.NextAction(agent);
            if (action != "null" && consoleLog)
                Debug.Log($"{agent.id}: {action}");

            switch (action)
            {
                case "bump_wall":
                    _prologInterface.RemoveCellContentKb("safe", agent.coord);
                    _world.Move(agent, agent.MoveBack());
                    break;
                case "pickup_gold":
                    agent.nbGold += 1;
                    _world.RemoveFromGrids(agent.coord, "gold", true, true);
                    _prologInterface.AddToKb($"nb_gold({agent.id}, {agent.nbGold})", true);
                    break;
                case "shoot_right":
                    _world.ShootArrow(agent, "right");
                    break;
                case "shoot_left":
                    _world.ShootArrow(agent, "left");
                    break;
                case "shoot_up":
                    _world.ShootArrow(agent, "up");
                    break;
                case "shoot_down":
                    _world.ShootArrow(agent, "down");
                    break;
                default:
                    actionLeftToDo = false;
                    break;
            }
        }
    }

    public void SetGameOver(string message, bool exitApp)
    {
        _gameOver = true;
        Debug.Log(message);
        // Debug.Log($"Game Duration: {_timer2}");

        if (!exitApp) return;

        Application.Quit();
        UnityEditor.EditorApplication.isPlaying = false;
    }
}