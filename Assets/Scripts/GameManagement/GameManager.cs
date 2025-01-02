using System;
using System.Collections.Generic;
using Prolog;
using UnityEngine;
using Random = UnityEngine.Random;

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
        [Range(0.0f, 1.0f)] public float timerInterval = 0.016f;

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
        public enum AIType { Prolog, BehaviourTree, LargeLanguageModel, Basic, FiniteStateMachine }
        public AIType aiType = AIType.Basic;
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
            var data = new List<Dictionary<string, object>>
        {
            new() {
                {"randomSeed", randomSeed},
                {"gridMin", gridMin},
                {"gridMax", gridMax},
                {"tileSize", tileSize},
                {"nbPit", nbPit},
                {"nbWumpus", nbWumpus},
                {"nbGold", nbGold},
                {"nbAgent", nbAgent},
                {"aiType", aiType.ToString()},
                {"personalities", personalities},
                {"agents", agents.Count},
                {"isGameOver", isGameOver},
                {"isModeAuto", isModeAuto},
                {"turnDuration", turnDuration},
            }
        };

            // Add individual agent data
            for (var i = 0; i < agents.Count; i++)
            {
                var agent = agents[i];
                var agentData = new Dictionary<string, object> {
                    {"agent", agent.name},
                    {"actions", agentAction.GetRange(i * turnDuration.Count, turnDuration.Count)},
                    {"positions", agentPosition.GetRange(i * turnDuration.Count, turnDuration.Count)},
                };
                data.Add(agentData);
            }

            // Serialize and save to file
            var json = System.Text.Json.JsonSerializer.Serialize(data, new System.Text.Json.JsonSerializerOptions { WriteIndented = true });
            // Create file /../data/DATE-AIType-Personalities.json and write json to it
            var path = $"data/{DateTime.Now:yy_MM_dd_HH_mm_ss}-{aiType}-{string.Join("-", personalities)}.json";
            System.IO.File.WriteAllText(path, json);
        }
    }
}