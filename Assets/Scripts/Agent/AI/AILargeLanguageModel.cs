using System.Collections;
using System.Collections.Generic;
using System.Text;
using GameManagement;
using UnityEngine;
using UnityEngine.Networking;
using System.Text.Json.Serialization;

// An AI controller that uses a Large Language Model (LLM) to decide the agent's actions.
// This class constructs a prompt based on the current game state, sends it to an LLM API,
// and parses the response to execute the next action. It supports multiple LLM providers.
//
// --- Usage Instructions ---
// 1.  Attach this script to the Agent GameObject in your Unity scene.
// 2.  In the Inspector, select the desired `ApiProvider` (OpenAI, Mistral, Ollama, or OpenRouter).
// 3.  If using OpenAI, Mistral, or OpenRouter, create a file named `env.txt` in the `Assets/Resources` folder.
// 4.  Add your API key to `env.txt` in the following format:
//     - For OpenAI: `OPENAI_API_KEY="your_api_key_here"`
//     - For Mistral: `MISTRAL_API_KEY="your_api_key_here"`
//     - For OpenRouter: `OPENROUTER_API_KEY="your_api_key_here"`
// 5.  If using Ollama, ensure your local Ollama server is running.
// 6.  The model names are hardcoded. You can change them in the `GetNextAction` method if needed.
namespace Agent.AI
{
    public class AILargeLanguageModel : AIBasic
    {
        // LLM and game state variables
        private string _apiKey;
        private readonly List<ApiMessage> _chatHistory = new();
        public IReadOnlyList<ApiMessage> ChatHistory => _chatHistory.AsReadOnly();
        private GameManager _gameManager;
        private readonly Dictionary<string, string> _envVars = new();

        // Gets the API endpoint URL based on the selected provider.
        private string ApiUrl
        {
            get
            {
                return _gameManager.apiProvider switch
                {
                    GameManager.ApiProvider.OpenAI => "https://api.openai.com/v1/chat/completions",
                    GameManager.ApiProvider.Mistral => "https://api.mistral.ai/v1/chat/completions",
                    GameManager.ApiProvider.OpenRouter => "https://openrouter.ai/api/v1/chat/completions",
                    GameManager.ApiProvider.Ollama => "http://localhost:11434/api/chat",
                    _ => null
                };
            }
        }

        // Represents a single message in the chat history for the API request.
        [System.Serializable]
        public class ApiMessage
        {
            [JsonInclude]
            public string role;
            [JsonInclude]
            public string content;
        }

        // Represents the request body for OpenAI and Mistral APIs.
        [System.Serializable]
        private class ApiRequestBody
        {
            public string model;
            public List<ApiMessage> messages;
        }

        // Represents the request body for the Ollama API.
        [System.Serializable]
        private class OllamaApiRequestBody
        {
            public string model;
            public List<ApiMessage> messages;
            public bool stream;
        }

        // Represents the API response from OpenAI and Mistral.
        [System.Serializable]
        private class ApiResponse
        {
            public List<ApiChoice> choices;
        }

        // Represents a single choice in the API response.
        [System.Serializable]
        private class ApiChoice
        {
            public ApiMessage message;
        }

        // Represents the API response from Ollama.
        [System.Serializable]
        private class OllamaApiResponse
        {
            public ApiMessage message;
        }

        // Initializes the component.
        private new void Awake()
        {
            base.Awake();
            _gameManager = GameManager.Instance;
            InitializePrompt();
        }

        // Loads environment variables and retrieves the API key.
        private void Start()
        {
            LoadEnv();
            if (_gameManager.apiProvider != GameManager.ApiProvider.Ollama)
            {
                _apiKey = GetEnvValue(_gameManager.apiProvider switch
                {
                    GameManager.ApiProvider.OpenAI => "OPENAI_API_KEY",
                    GameManager.ApiProvider.Mistral => "MISTRAL_API_KEY",
                    GameManager.ApiProvider.OpenRouter => "OPENROUTER_API_KEY",
                    _ => ""
                });
            }
        }

        // Loads environment variables from the `env.txt` file in the Resources folder.
        private void LoadEnv()
        {
            var envText = Resources.Load<TextAsset>("env")?.text;
            if (string.IsNullOrEmpty(envText))
            {
                Debug.LogWarning("env.txt file not found or is empty in Resources folder.");
                return;
            }

            foreach (var line in envText.Split('\n'))
            {
                var parts = line.Split(new[] { '=' }, 2);
                if (parts.Length == 2)
                {
                    _envVars[parts[0].Trim()] = parts[1].Trim().Trim('"');
                }
            }
        }

        // Retrieves a value from the loaded environment variables.
        private string GetEnvValue(string key)
        {
            return _envVars.TryGetValue(key, out var value) ? value : null;
        }

        // Initializes the chat history with a system prompt and few-shot examples.
        private void InitializePrompt()
        {
            _chatHistory.Clear();
            _chatHistory.Add(new ApiMessage { role = "system", content = @"
You are a cautious and strategic agent in a grid-based game. Your primary goal is to find the gold and return to the starting cell (1,1) without dying.

**--- Rules of the World ---**
1.  **Objective:** Find the gold, pick it up, and navigate back to the starting cell (1,1) to win.
2.  **Movement:** You can move one cell at a time: `Move Right`, `Move Left`, `Move Up`, `Move Down`.
3.  **Hazards:**
    *   **Pits:** If you enter a cell with a pit, you die. A 'Breeze' in your current cell means an adjacent cell contains a pit.
    *   **Wumpus:** If you enter a cell with the Wumpus, you die. A 'Stench' in your current cell means the Wumpus is in an adjacent cell.
4.  **Shooting:**
    *   You have one arrow. You can `Shoot Right`, `Shoot Left`, `Shoot Up`, or `Shoot Down`.
    *   Shooting into a cell with the Wumpus will kill it, removing the threat.
    *   Shooting at a wall or an empty cell wastes the arrow.
5.  **Safety:** Your top priority is survival. Always choose a safe move over a risky one. A cell is known to be safe if you have visited it, or if it's adjacent to a cell with no breeze or stench.

**--- Your Task ---**
You will be given the current game state each turn. You must respond with only the action to take.

**--- Response Format ---**
Your response must be a single line with the action and direction, like `Move Right` or `Shoot Up`.
" });
            // Example Game
            _chatHistory.Add(new ApiMessage { role = "user", content = @"New Turn:
Current position: (1, 1)
Has gold: false
Known map:
Visited cells: (1,1) 
Safe cells: (1,2) (2,1) 
Cells with breeze: 
Cells with stench: 
Perceived Wumpus cells: 
Perceived wall cells: 
What is your next action?" });
            _chatHistory.Add(new ApiMessage { role = "assistant", content = "Move Right" });

            _chatHistory.Add(new ApiMessage { role = "user", content = @"New Turn:
Current position: (2, 1)
Has gold: false
Known map:
Visited cells: (1,1) (2,1) 
Safe cells: (1,2) (1,1) 
Cells with breeze: (2,1) 
Cells with stench: 
Perceived Wumpus cells: 
Perceived wall cells: 
What is your next action?" });
            _chatHistory.Add(new ApiMessage { role = "assistant", content = "Move Left" });

            _chatHistory.Add(new ApiMessage { role = "user", content = @"New Turn:
Current position: (1, 1)
Has gold: false
Known map:
Visited cells: (1,1) (2,1) 
Safe cells: (1,2) (2,1) 
Cells with breeze: (2,1) 
Cells with stench: 
Perceived Wumpus cells: 
Perceived wall cells: 
What is your next action?" });
            _chatHistory.Add(new ApiMessage { role = "assistant", content = "Move Up" });
        }

        // Executes the agent's turn.
        public override void PlayTurn()
        {
            StartCoroutine(PlayTurnCoroutine());
        }

        // Coroutine to build the prompt and request the next action from the LLM.
        private IEnumerator PlayTurnCoroutine()
        {
            var prompt = BuildPrompt();
            yield return StartCoroutine(GetNextAction(prompt, ExecuteAction));
        }

        // Constructs the user prompt for the current turn.
        private string BuildPrompt()
        {
            var promptBuilder = new StringBuilder();
            promptBuilder.AppendLine("New Turn:");
            promptBuilder.Append(GetGameState());
            promptBuilder.AppendLine("What is your next action?");
            return promptBuilder.ToString();
        }

        // Gathers and formats the current game state.
        private string GetGameState()
        {
            var gameStateBuilder = new StringBuilder();
            gameStateBuilder.AppendLine($"Current position: ({_agent.coords.x}, {_agent.coords.y})");
            gameStateBuilder.AppendLine($"Has gold: {_agent.nbGold > 0}");
            gameStateBuilder.AppendLine("Known map:");

            var visitedCells = new StringBuilder("Visited cells: ");
            var safeCells = new StringBuilder("Safe cells: ");
            var breezeCells = new StringBuilder("Cells with breeze: ");
            var stenchCells = new StringBuilder("Cells with stench: ");
            var wumpusCells = new StringBuilder("Perceived Wumpus cells: ");
            var wallCells = new StringBuilder("Perceived wall cells: ");

            for (var i = 0; i < _gameManager.gridMax.x; i++)
            {
                for (var j = 0; j < _gameManager.gridMax.y; j++)
                {
                    if (_gameManager.AgentsMap[i, j] == null) continue;
                    var cellContents = new List<string>();
                    foreach (var go in _gameManager.AgentsMap[i, j])
                    {
                        cellContents.Add(go.tag);
                    }

                    if (cellContents.Contains("VisitedCell")) visitedCells.Append($"({i},{j}) ");
                    if (cellContents.Contains("SafeCell")) safeCells.Append($"({i},{j}) ");
                    if (cellContents.Contains("Breeze")) breezeCells.Append($"({i},{j}) ");
                    if (cellContents.Contains("Stench")) stenchCells.Append($"({i},{j}) ");
                    if (cellContents.Contains("Wumpus")) wumpusCells.Append($"({i},{j}) ");
                    if (cellContents.Contains("Wall")) wallCells.Append($"({i},{j}) ");
                }
            }

            gameStateBuilder.AppendLine(visitedCells.ToString());
            gameStateBuilder.AppendLine(safeCells.ToString());
            gameStateBuilder.AppendLine(breezeCells.ToString());
            gameStateBuilder.AppendLine(stenchCells.ToString());
            gameStateBuilder.AppendLine(wumpusCells.ToString());
            gameStateBuilder.AppendLine(wallCells.ToString());

            return gameStateBuilder.ToString();
        }

        // Sends the prompt to the LLM API and handles the response.
        private IEnumerator GetNextAction(string prompt, System.Action<string> callback)
        {
            _chatHistory.Add(new ApiMessage { role = "user", content = prompt });

            var modelName = _gameManager.apiProvider switch
            {
                GameManager.ApiProvider.OpenAI => "o4-mini",
                // GameManager.ApiProvider.OpenAI => "gpt-4o-mini",
                // GameManager.ApiProvider.Mistral => "mistral-medium-latest",
                GameManager.ApiProvider.Mistral => "magistral-medium-latest",
                GameManager.ApiProvider.OpenRouter => "deepseek/deepseek-r1:free",
                // GameManager.ApiProvider.Ollama => "deepseek-r1:1.5b",
                // GameManager.ApiProvider.Ollama => "deepseek-r1:14b",
                GameManager.ApiProvider.Ollama => "gemma3n:e4b",
                _ => "o4-mini"
            };

            string requestJson;
            if (_gameManager.apiProvider == GameManager.ApiProvider.Ollama)
            {
                var requestBody = new OllamaApiRequestBody
                {
                    model = modelName,
                    messages = _chatHistory,
                    stream = false
                };
                requestJson = JsonUtility.ToJson(requestBody);
            }
            else
            {
                var requestBody = new ApiRequestBody
                {
                    model = modelName,
                    messages = _chatHistory
                };
                requestJson = JsonUtility.ToJson(requestBody);
            }

            var bodyRaw = Encoding.UTF8.GetBytes(requestJson);

            Debug.Log($"API Request: {requestJson}");

            using var request = new UnityWebRequest(ApiUrl, "POST");
            request.uploadHandler = new UploadHandlerRaw(bodyRaw);
            request.downloadHandler = new DownloadHandlerBuffer();
            request.SetRequestHeader("Content-Type", "application/json");

            if (_gameManager.apiProvider != GameManager.ApiProvider.Ollama)
            {
                request.SetRequestHeader("Authorization", $"Bearer {_apiKey}");
            }

            if (_gameManager.apiProvider == GameManager.ApiProvider.OpenRouter)
            {
                request.SetRequestHeader("HTTP-Referer", "https://github.com/lapeyrade/Wumpus_World");
                request.SetRequestHeader("X-Title", "Wumpus World");
            }

            yield return request.SendWebRequest();

            if (request.result is UnityWebRequest.Result.ConnectionError or UnityWebRequest.Result.ProtocolError)
            {
                Debug.LogError(request.error);
                Debug.LogError(request.downloadHandler.text);
                _chatHistory.RemoveAt(_chatHistory.Count - 1); // Remove failed user message
            }
            else
            {
                var responseJson = request.downloadHandler.text;
                Debug.Log($"API Response: {responseJson}");

                string responseMessage;

                if (_gameManager.apiProvider == GameManager.ApiProvider.Ollama)
                {
                    var response = JsonUtility.FromJson<OllamaApiResponse>(responseJson);
                    responseMessage = response.message.content.Trim();
                }
                else
                {
                    var response = JsonUtility.FromJson<ApiResponse>(responseJson);
                    responseMessage = response.choices[0].message.content.Trim();
                }

                _chatHistory.Add(new ApiMessage { role = "assistant", content = responseMessage });
                
                // Save the updated chat history after each turn
                _gameManager.SaveLlmData(gameObject);
                
                callback?.Invoke(responseMessage);
            }
        }

        // Parses the LLM response and executes the corresponding game action.
        private void ExecuteAction(string action)
        {
            var moveDirection = Vector2Int.zero;
            switch (action)
            {
                case "Move Right":
                    moveDirection = Vector2Int.right;
                    break;
                case "Move Left":
                    moveDirection = Vector2Int.left;
                    break;
                case "Move Up":
                    moveDirection = Vector2Int.up;
                    break;
                case "Move Down":
                    moveDirection = Vector2Int.down;
                    break;
                case "Pick up gold":
                    _agentAction.PickUpGold();
                    break;
                case "Shoot Right":
                    _agentAction.ShootArrow(Vector2Int.right);
                    break;
                case "Shoot Left":
                    _agentAction.ShootArrow(Vector2Int.left);
                    break;
                case "Shoot Up":
                    _agentAction.ShootArrow(Vector2Int.up);
                    break;
                case "Shoot Down":
                    _agentAction.ShootArrow(Vector2Int.down);
                    break;
                default:
                    Debug.LogWarning($"Unknown action: {action}. Doing nothing.");
                    break;
            }

            if (moveDirection != Vector2Int.zero)
            {
                _agentMove.MoveAgent(_agent.coords + moveDirection);
            }

            _agentSense.SenseCell();
        }
    }
}