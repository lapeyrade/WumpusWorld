using System;
using System.Collections.Generic;
using Prolog;
using UnityEngine;
using Random = UnityEngine.Random;

namespace GameManagement
{
    public class GameManager : MonoBehaviour
    {
        // Singleton instance
        public static GameManager Instance;

        // Cached components
        private PrologInterface _prologInterface;
        private GridBuilder _gridBuilder;
        private CameraController _cameraController;

        // Game status and settings
        public bool saveData;
        public bool isGameOver;
        public bool isModeAuto;
        [Range(0.0f, 1.0f)] public float timerInterval = 0.016f;

        // Grid settings
        public int randomSeed = 1;
        public Vector2Int gridMin = new(0, 0);
        public Vector2Int gridMax = new(15, 15);
        public float tileSize = 1.05f;

        // Element counts
        public int nbPit = 4;
        public int nbWumpus = 2;
        public int nbGold = 1;
        public int nbAgent = 2;

        // AI settings
        public enum AIType { Prolog, BehaviourTree, LargeLanguageModel, Basic, FiniteStateMachine }
        public AIType aiType = AIType.Basic;
        public enum Personalities { Cupid, Ascetic, Brave, Coward }
        public List<Personalities> personalities = new() { Personalities.Brave };

        // Game objects
        public List<GameObject> agents;
        public List<GameObject>[,] Map;
        public List<GameObject>[,] AgentsMap;

        // Game data
        public List<float> turnDuration = new();
        public List<string> agentAction = new();
        public List<Vector2Int> agentPosition = new();

        // Initialize GameManager and its components
        protected void Awake()
        {
            // Set up singleton instance
            Instance = this;

            // Cache components
            if (aiType is AIType.Prolog)
                _prologInterface = GetComponent<PrologInterface>();
            _gridBuilder = GetComponent<GridBuilder>();
            _cameraController = GameObject.Find("Main Camera").GetComponent<CameraController>();

            // Initialize random seed
            Random.InitState(randomSeed);

            // Initialize game object grids
            Map = new List<GameObject>[gridMax.x, gridMax.y];
            AgentsMap = new List<GameObject>[gridMax.x, gridMax.y];

            // Initialize agents list
            agents = new List<GameObject>();

            // Add grid manager and grid builder
            gameObject.AddComponent<GridManager>();
            gameObject.AddComponent<GridBuilder>();
            
            // Adjust camera
            _cameraController.AdjustCameraPosition();

            // Add GameController component
            gameObject.AddComponent<GameController>();
        }

        // Set game over status and optionally exit the application
        public void SetGameOver(bool exitApp)
        {
            isGameOver = true;

            if (saveData) SaveData();

            if (!exitApp) return;

            Application.Quit();
            UnityEditor.EditorApplication.isPlaying = false;
        }

        // Save game data to JSON file for analysis
        private void SaveData()
        {
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

            // for each agent add all actions and positions
            for (var i = 0; i < agents.Count; i++)
            {
                var agent = agents[i];
                var agentData = new Dictionary<string, object>
            {
                {"agent", agent.name},
                {"actions", agentAction.GetRange(i * turnDuration.Count, turnDuration.Count)},
                {"positions", agentPosition.GetRange(i * turnDuration.Count, turnDuration.Count)},
            };
                data.Add(agentData);
            }

            var json = System.Text.Json.JsonSerializer.Serialize(data, new System.Text.Json.JsonSerializerOptions { WriteIndented = true });
            // Create file /../data/DATE-AIType-Personalities.json and write json to it
            var path = $"data/{DateTime.Now:yy_MM_dd_HH_mm_ss}-{aiType}-{string.Join("-", personalities)}.json";
            System.IO.File.WriteAllText(path, json);
        }
    }
}