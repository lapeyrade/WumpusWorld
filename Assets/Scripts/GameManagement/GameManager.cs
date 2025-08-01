using System;
using System.Collections.Generic;
using System.Linq;
using Prolog;
using UnityEngine;
using Random = UnityEngine.Random;
using System.Text.Json;

namespace GameManagement
{
    public class GameManager : MonoBehaviour
    {
        // Singleton instance for global access
        public static GameManager Instance;

        // Core component references
        private PrologInterface _prologInterface;
        private GridBuilder _gridBuilder;
        private CameraController _cameraController;

        // Game status and settings
        public bool saveData;
        public bool isGameOver;
        public bool isModeAuto;
        [Range(0.0f, 1.0f)] public float timerInterval = 0.0f;

        // Grid configuration
        public int randomSeed = 1;           // Seed for reproducible randomization
        public Vector2Int gridMin = new(0, 0);    // Grid boundaries
        public Vector2Int gridMax = new(15, 15);
        public float tileSize = 1.05f;       // Size of each grid tile

        // Entity counts
        public int nbPit = 4;       // Number of pits to generate
        public int nbWumpus = 2;    // Number of Wumpuses
        public int nbGold = 1;      // Number of gold pieces
        public int nbAgent = 2;     // Number of agents

        // AI configuration
        public enum AIType { Prolog, BehaviorTree, LargeLanguageModel, Basic, FiniteStateMachine }
        public AIType aiType = AIType.Basic;
        public enum ApiProvider { OpenAI, Mistral, Ollama, OpenRouter }
        public ApiProvider apiProvider = ApiProvider.OpenAI;
        public enum Personalities { Cupid, Ascetic, Brave, Coward }
        public List<Personalities> personalities = new() { Personalities.Brave };

        // Game object collections
        public List<GameObject> agents;                // Active agents
        public List<GameObject>[,] Map;               // Main game grid
        public List<GameObject>[,] AgentsMap;         // Agent knowledge grid

        // Game metrics and history
        public List<decimal> turnDuration = new();      // Performance tracking
        public List<string> agentAction = new();      // Action history
        public List<Vector2Int> agentPosition = new();// Position history

        // Initialize GameManager and its components
        protected void Awake()
        {
            // Set up singleton instance
            Instance = this;

            // Cache required components
            if (aiType is AIType.Prolog)
                _prologInterface = gameObject.AddComponent<PrologInterface>();
            _gridBuilder = GetComponent<GridBuilder>();
            _cameraController = GameObject.Find("Main Camera").GetComponent<CameraController>();

            // Initialize random number generation
            Random.InitState(randomSeed);

            // Initialize game grids
            Map = new List<GameObject>[gridMax.x, gridMax.y];
            AgentsMap = new List<GameObject>[gridMax.x, gridMax.y];
            agents = new List<GameObject>();

            // Add required game systems
            gameObject.AddComponent<GridManager>();
            gameObject.AddComponent<GridBuilder>();

            // Set up camera view
            _cameraController.AdjustCameraPosition();

            // Initialize game controller
            gameObject.AddComponent<GameController>();
        }

        // Handle game over state and cleanup
        public void SetGameOver(bool exitApp)
        {
            isGameOver = true;

            // Save game data if enabled
            if (saveData) SaveData();

            // Exit application if requested
            if (!exitApp) return;

            Application.Quit();
            UnityEditor.EditorApplication.isPlaying = false;
        }

        // Save game metrics and history to JSON file
        private void SaveData()
        {
            // Prepare game configuration data
            var gameConfig = new Dictionary<string, object>
            {
                { "randomSeed", randomSeed },
                { "gridMin", new { x = gridMin.x, y = gridMin.y } },
                { "gridMax", new { x = gridMax.x, y = gridMax.y } },
                { "tileSize", Math.Round(tileSize, 2) },
                { "nbPit", nbPit },
                { "nbWumpus", nbWumpus },
                { "nbGold", nbGold },
                { "nbAgent", nbAgent },
                { "aiType", aiType.ToString() },
                { "personalities", personalities },
                { "agents", agents.Count },
                { "isGameOver", isGameOver },
                { "isModeAuto", isModeAuto },
                { "turnDuration", turnDuration.Select(d => Math.Round(d, 2)).ToList() }
            };
            if (aiType is AIType.LargeLanguageModel) gameConfig.Add("apiProvider", apiProvider.ToString());

            var data = new List<Dictionary<string, object>> { gameConfig };

            // Add individual agent data
            for (var i = 0; i < agents.Count; i++)
            {
                var agent = agents[i];
                var agentData = new Dictionary<string, object> {
                    {"agent", agent.name},
                    {"actions", agentAction.GetRange(i * turnDuration.Count, turnDuration.Count)},
                    {"positions", agentPosition.GetRange(i * turnDuration.Count, turnDuration.Count)
                        .Select(v => new { x = v.x, y = v.y }).ToList()},
                };
                data.Add(agentData);
            }

            // Serialize and save to file
            var json = System.Text.Json.JsonSerializer.Serialize(data, new System.Text.Json.JsonSerializerOptions { WriteIndented = true });
            // Create file /../data/DATE-AIType-Personalities.json and write json to it
            // var path = $"data/{DateTime.Now:yy_MM_dd_HH_mm_ss}-{aiType}-{string.Join("-", personalities)}.json";
            var path = $"data/{gridMax.x}x{gridMax.y}-{nbAgent}a-{nbWumpus}wp-{nbGold}g-{string.Join("-", personalities)}-{aiType}.json";
            System.IO.File.WriteAllText(path, json);
        }
        
        public void SaveLlmData(GameObject currentAgent)
        {
            if (aiType != AIType.LargeLanguageModel) return;
            var path = $"data/{gridMax.x}x{gridMax.y}-{nbAgent}a-{nbWumpus}wp-{nbGold}g-{string.Join("-", personalities)}-{aiType}.json";

            List<Dictionary<string, object>> data;
            if (System.IO.File.Exists(path))
            {
                var existingJson = System.IO.File.ReadAllText(path);
                var options = new JsonSerializerOptions
                {
                    PropertyNameCaseInsensitive = true
                };
                data = JsonSerializer.Deserialize<List<Dictionary<string, object>>>(existingJson, options);
            }
            else
            {
                data = new List<Dictionary<string, object>>();
                var gameConfig = new Dictionary<string, object>
                {
                    { "randomSeed", randomSeed },
                    { "gridMin", new { x = gridMin.x, y = gridMin.y } },
                    { "gridMax", new { x = gridMax.x, y = gridMax.y } },
                    { "tileSize", Math.Round(tileSize, 2) },
                    { "nbPit", nbPit },
                    { "nbWumpus", nbWumpus },
                    { "nbGold", nbGold },
                    { "nbAgent", nbAgent },
                    { "aiType", aiType.ToString() },
                    { "personalities", personalities },
                    { "agents", agents.Count },
                    { "isGameOver", isGameOver },
                    { "isModeAuto", isModeAuto },
                    { "apiProvider", apiProvider.ToString() }
                };
                data.Add(gameConfig);
            }

            var agentData = data.FirstOrDefault(d => d.ContainsKey("agent") && ((JsonElement)d["agent"]).GetString() == currentAgent.name);

            var llm = currentAgent.GetComponent<Agent.AI.AILargeLanguageModel>();
            if (llm != null)
            {
                if (agentData == null)
                {
                    agentData = new Dictionary<string, object> { { "agent", currentAgent.name } };
                    data.Add(agentData);
                }
                agentData["chatHistory"] = llm.ChatHistory;
            }

            var json = JsonSerializer.Serialize(data,
                new JsonSerializerOptions { WriteIndented = true, Converters = { new System.Text.Json.Serialization.JsonStringEnumConverter() }});
            System.IO.File.WriteAllText(path, json);
        }
    }
}