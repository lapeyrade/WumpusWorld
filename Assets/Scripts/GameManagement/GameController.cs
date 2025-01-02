using UnityEngine;
using Prolog;
using Agent.AI;
using System;

namespace GameManagement
{
    public class GameController : MonoBehaviour
    {
        // Core game timers and management
        private float _timer;
        private GameManager _gameManager;
        private PrologInterface _prologInterface;

        // Agent component arrays
        private Agent.Agent[] _agent;
        private AIBasic[] _aiComponents;

        private void Awake()
        {
            _gameManager = GameManager.Instance;
            if (GameManager.Instance.aiType is GameManager.AIType.Prolog)
                _prologInterface = _gameManager.GetComponent<PrologInterface>();
        }

        // Initialize game state and agent components
        protected void Start()
        {
            // Cache agent components for efficient access
            _agent = new Agent.Agent[_gameManager.agents.Count];
            _aiComponents = new AIBasic[_gameManager.agents.Count];

            // Initialize each agent's components and execute their first turn
            for (int i = 0; i < _gameManager.agents.Count; i++)
            {
                _agent[i] = _gameManager.agents[i].GetComponent<Agent.Agent>();
                _aiComponents[i] = _gameManager.agents[i].GetComponent<AIBasic>();
                _aiComponents[i].FirstTurn();
            }

            // Initialize Prolog knowledge base if using Prolog AI
            if (_gameManager.aiType is GameManager.AIType.Prolog)
                _prologInterface.RunQuery();
        }

        // Main game loop
        protected void Update()
        {
            // Handle automatic turn execution based on timer
            if (_gameManager.isModeAuto && _timer < _gameManager.timerInterval)
                _timer += Time.deltaTime;
            else
            {
                PlayTurn();
                _timer = 0.0f;
            }
        }

        // Execute agents' turns based on input or auto mode
        private void PlayTurn()
        {
            var watch = System.Diagnostics.Stopwatch.StartNew();

            // Skip turn if no input in manual mode or game is over
            if ((!Input.anyKeyDown && !_gameManager.isModeAuto) || _gameManager.isGameOver) return;

            // Handle special key inputs
            if (Input.GetKeyDown(KeyCode.Escape))
                _gameManager.SetGameOver(true);
            else if (Input.GetKeyDown(KeyCode.Backspace))
            {
                ScreenCapture.CaptureScreenshot("Screenshots/screenshot " + System.DateTime.Now.ToString("MM-dd-yy (HH-mm-ss)") + ".png");
                Debug.Log("Screenshot saved!");
            }
            // Execute turn on movement keys, space, return, or in auto mode
            else if (Input.GetKeyDown(KeyCode.Return) || Input.GetKeyDown(KeyCode.Space) ||
                     Input.GetKeyDown(KeyCode.RightArrow) || Input.GetKeyDown(KeyCode.LeftArrow) ||
                     Input.GetKeyDown(KeyCode.UpArrow) || Input.GetKeyDown(KeyCode.DownArrow) ||
                     _gameManager.isModeAuto)
            {
                // Process each agent's turn
                for (int i = 0; i < _aiComponents.Length; i++)
                {
                    var agent = _aiComponents[i]._agent;

                    // Skip agent's turn if no safe cells are available from start position
                    if (_gameManager.AgentsMap[agent.coords.x, agent.coords.y].Exists(e => e.tag is "StartCell")
                        && !_gameManager.AgentsMap[agent.coords.x + 1, agent.coords.y].Exists(e => e.tag is "SafeCell")
                        && !_gameManager.AgentsMap[agent.coords.x - 1, agent.coords.y].Exists(e => e.tag is "SafeCell")
                        && !_gameManager.AgentsMap[agent.coords.x, agent.coords.y + 1].Exists(e => e.tag is "SafeCell")
                        && !_gameManager.AgentsMap[agent.coords.x, agent.coords.y - 1].Exists(e => e.tag is "SafeCell"))
                        continue;

                    _aiComponents[i].PlayTurn();
                }

                // Update Prolog knowledge base if using Prolog AI
                if (_gameManager.aiType is GameManager.AIType.Prolog)
                    _prologInterface.RunQuery();
            }

            // Record performance metrics and agent state
            watch.Stop();
            Debug.Log($"Execution Time: {watch.ElapsedTicks / 10000.0:F3} ms");
            _gameManager.turnDuration.Add(decimal.Round((decimal)(watch.ElapsedTicks / 10000.0), 3));

            // Update game state with each agent's position and last action
            for (int i = 0; i < _agent.Length; i++)
            {
                _gameManager.agentPosition.Add(_agent[i].coords);
                _gameManager.agentAction.Add(_agent[i].lastAction);
            }
        }
    }
}