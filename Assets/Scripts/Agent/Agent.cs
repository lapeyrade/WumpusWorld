using System;
using System.Collections.Generic;
using System.Linq;
using Agent.AI;
using UnityEngine;
using GameManagement;

namespace Agent
{
    public class Agent : MonoBehaviour
    {
        public Vector2Int startCoord;
        public Vector2Int coords;
        public int nbGold;
        public int nbArrow;

        public GameObject prefabAgentWorld;
        public GameObject prefabGoldAgent;
        public GameObject prefabGoldMap;

        protected internal readonly Stack<Vector2Int> PastMovements = new();

        public string lastAction;

        // Initialize agent with given parameters
        public void Init(int agentId, Vector2Int newCoords, int nbTotalWumpus)
        {
            SetAgentInfo(agentId, newCoords);
            nbArrow = nbTotalWumpus;
            InitializeAgentPrefab();

            AttachAIComponent();
            AttachPersonalityComponents();
        }

        // Set agent's basic information
        private void SetAgentInfo(int agentId, Vector2Int newCoords)
        {
            name = $"human{agentId}";
            tag = "Human";
            startCoord = newCoords;
            coords = newCoords;
            PastMovements.Push(coords);
            transform.position = GridManager.GetAgentMapOffset(newCoords);
        }

        // Instantiate and configure the agent prefab in the world
        private void InitializeAgentPrefab()
        {
            prefabAgentWorld = Instantiate(Resources.Load("Human"), transform) as GameObject;
            if (prefabAgentWorld == null) return;
            prefabAgentWorld.tag = tag;
            prefabAgentWorld.name = name;
            prefabAgentWorld.transform.position = GridManager.GetWorldMapOffset(coords);
        }

        // Attach the appropriate AI component based on the game manager's settings
        private void AttachAIComponent()
        {
            var aiComponentType = GameManager.Instance.aiType switch
            {
                GameManager.AIType.Prolog => typeof(AIProlog),
                GameManager.AIType.BehaviourTree => typeof(AIBehaviourTree),
                GameManager.AIType.FiniteStateMachine => typeof(AIFiniteStateMachine),
                GameManager.AIType.LargeLanguageModel => typeof(AILargeLanguageModel),
                _ => typeof(AIBasic),
            };
            gameObject.AddComponent(aiComponentType);
        }

        // Attach the personality components based on the game manager's settings
        private void AttachPersonalityComponents()
        {
            foreach (var personalityType in GameManager.Instance.personalities.Select(personality =>
                         Type.GetType("Ontology." + personality)))
                gameObject.AddComponent(personalityType);
        }
    }
}
