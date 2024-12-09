using UnityEngine;
using Prolog;
using Agent.AI;

namespace GameManagement
{
    public class GameController : MonoBehaviour
    {
        private float _timer;
        private GameManager _gameManager;
        private PrologInterface _prologInterface;
        private Agent.Agent[] _agentComponents;
        private AIBasic[] _aiComponents;

        private void Awake()
        {
            _gameManager = GameManager.Instance;
            _prologInterface = _gameManager.GetComponent<PrologInterface>();
        }

        // Called before the first frame update
        protected void Start()
        {
            // Cache agent components
            _agentComponents = new Agent.Agent[_gameManager.agents.Count];
            _aiComponents = new AIBasic[_gameManager.agents.Count];
            
            for (int i = 0; i < _gameManager.agents.Count; i++)
            {
                _agentComponents[i] = _gameManager.agents[i].GetComponent<Agent.Agent>();
                _aiComponents[i] = _gameManager.agents[i].GetComponent<AIBasic>();
                _aiComponents[i].FirstTurn();
            }
            
            if (_gameManager.aiType is GameManager.AIType.Prolog)
                _prologInterface.RunQuery();
        }

        // Called once per frame
        protected void Update()
        {
            // Increment timer if in auto mode, otherwise reset it and call PlayTurn()
            if (_gameManager.isModeAuto && _timer < _gameManager.timerInterval)
                _timer += Time.deltaTime;
            else
            {
                PlayTurn();
                _timer = 0.0f;
            }
        }

        // Execute agents' turns based on input
        private void PlayTurn()
        {
            var watch = System.Diagnostics.Stopwatch.StartNew();

            // Return if no relevant input or game is over
            if ((!Input.anyKeyDown && !_gameManager.isModeAuto) || _gameManager.isGameOver) return;

            // Check for specific key inputs and perform corresponding actions
            if (Input.GetKeyDown(KeyCode.Escape))
                _gameManager.SetGameOver(true);
            else if (Input.GetKeyDown(KeyCode.Backspace))
            {
                ScreenCapture.CaptureScreenshot("Screenshots/screenshot " +
                                                System.DateTime.Now.ToString("MM-dd-yy (HH-mm-ss)") + ".png");
                Debug.Log("Screenshot saved!");
            }
            // Play turn if specific keys are pressed or game is in auto mode
            else if (Input.GetKeyDown(KeyCode.Return) || Input.GetKeyDown(KeyCode.Space) || Input.GetKeyDown(KeyCode.RightArrow) ||
                     Input.GetKeyDown(KeyCode.LeftArrow) || Input.GetKeyDown(KeyCode.UpArrow) || Input.GetKeyDown(KeyCode.DownArrow) ||
                     _gameManager.isModeAuto)
            {
                // Execute agents' turns and measure the execution time
                for (int i = 0; i < _aiComponents.Length; i++)
                {
                    _aiComponents[i].PlayTurn();
                }

                if (_gameManager.aiType is GameManager.AIType.Prolog)
                    _prologInterface.RunQuery();
            }

            // Update execution time and agent game data
            watch.Stop();
            Debug.Log($"Execution Time: {watch.ElapsedMilliseconds} ms");
            _gameManager.turnDuration.Add(watch.ElapsedMilliseconds);

            // for each agent update coords and last action
            for (int i = 0; i < _agentComponents.Length; i++)
            {
                _gameManager.agentPosition.Add(_agentComponents[i].coords);
                _gameManager.agentAction.Add(_agentComponents[i].lastAction);
            }
        }
    }
}