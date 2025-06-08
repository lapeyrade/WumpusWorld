using System;
using System.Collections.Generic;
using System.Linq;
using Agent.AI;
using GameManagement;
using Ontology;
using UnityEngine;

namespace Agent
{
    public class Agent : MonoBehaviour
    {
        // Core game references
        private GameManager _gameManager;

        // Component collections
        private readonly Dictionary<string, Personality> _personalities = new();
        private readonly Dictionary<string, Objective> _objectives = new();
        private readonly Dictionary<string, Ontology.Action> _actions = new();

        // Agent state properties
        public Vector2Int startCoord;
        public Vector2Int coords;
        public int nbGold;
        public int nbArrow;

        // Prefab references
        public GameObject prefabAgentWorld;
        public GameObject prefabGoldAgent;
        public GameObject prefabGoldMap;

        // Movement history
        protected internal readonly Stack<Vector2Int> PastMovements = new();

        // Last performed action
        public string lastAction;

        // Initialize agent with given parameters
        public void Init(int agentId, Vector2Int newCoords, int nbTotalWumpus)
        {
            _gameManager = GameManager.Instance;
            SetAgentInfo(agentId, newCoords);
            nbArrow = nbTotalWumpus;
            InitializeAgentPrefab();

            AttachAIComponent();
            AttachPersonalityComponents();
            AttachObjectiveComponents();
            AttachActionComponents();
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
            var aiComponentType = _gameManager.aiType switch
            {
                GameManager.AIType.Prolog => typeof(AIProlog),
                GameManager.AIType.BehaviorTree => typeof(AIBehaviorTree),
                GameManager.AIType.FiniteStateMachine => typeof(AIFiniteStateMachine),
                GameManager.AIType.LargeLanguageModel => typeof(AILargeLanguageModel),
                _ => typeof(AIBasic),
            };
            gameObject.AddComponent(aiComponentType);
        }

        // Attach the personality components based on the game manager's settings
        private void AttachPersonalityComponents()
        {
            foreach (var personality in _gameManager.personalities)
            {
                _personalities[personality.ToString()] =
                    gameObject.AddComponent(Type.GetType("Ontology." + personality)) as Personality;
            }
        }

        // Attach all concrete Objective components
        private void AttachObjectiveComponents()
        {
            var objectiveTypes = typeof(Objective).Assembly.GetTypes()
                .Where(t => t.IsClass && !t.IsAbstract && t != typeof(Objective)
                && typeof(Objective).IsAssignableFrom(t));

            foreach (var type in objectiveTypes)
            {
                if (gameObject.AddComponent(type) is Objective component)
                {
                    component.enabled = false;
                    _objectives[type.Name] = component;
                }
            }
        }

        // Attach all concrete Action components
        private void AttachActionComponents()
        {
            var actionTypes = typeof(Ontology.Action).Assembly.GetTypes()
                .Where(t => t.IsClass && !t.IsAbstract && t != typeof(Ontology.Action)
                && typeof(Ontology.Action).IsAssignableFrom(t));

            foreach (var type in actionTypes)
            {
                if (gameObject.AddComponent(type) is Ontology.Action component)
                {
                    component.enabled = false;
                    _actions[type.Name] = component;
                }
            }
        }

        // Get a specific personality component by type
        // Returns null if no matching enabled personality is found
        public T GetPersonality<T>() where T : Personality
        {
            foreach (var personality in _personalities.Values)
            {
                if (!personality.enabled) continue;

                // Check if personality matches requested type or is parent class
                if (personality is T matchingPersonality)
                    return matchingPersonality;
            }
            return null;
        }

        // Get a specific objective component by type
        // Returns null if no matching enabled objective is found
        public T GetObjective<T>() where T : Objective
        {
            foreach (var objective in _objectives.Values)
            {
                if (!objective.enabled) continue;

                // Check if objective matches requested type or is parent class
                if (objective is T matchingObjective)
                    return matchingObjective;
            }
            return null;
        }

        // Set a specific objective component to enabled/disabled
        public void SetObjective<T>(bool enabled) where T : Objective
        {
            if (_objectives.Values.FirstOrDefault(o => o is T) is T matchingObjective)
                matchingObjective.enabled = enabled;
        }

        // Set all objectives to false
        public void ResetObjectives() => _objectives.Values.ToList().ForEach(obj => obj.enabled = false);


        // Get a specific action component by type
        // Returns null if no matching enabled action is found
        public T GetAction<T>() where T : Ontology.Action
        {
            foreach (var action in _actions.Values)
            {
                if (!action.enabled) continue;

                // Check if action matches requested type or is parent class
                if (action is T matchingAction)
                    return matchingAction;
            }
            return null;
        }

        // Returns the action with the highest utility value from all available actions
        public Ontology.Action GetHighestUtilityAction() =>
            _actions.Values.OrderByDescending(a => a.Utility).First();

        // Set a specific action component to enabled/disabled
        public void SetAction<T>(bool enabled) where T : Ontology.Action
        {
            if (_actions.Values.FirstOrDefault(a => a is T) is T matchingAction)
                matchingAction.enabled = enabled;
        }

        // Set all actions to false and reset their utility
        public void ResetActions() =>
            _actions.Values.ToList().ForEach(obj => { obj.enabled = false; obj.Utility = 0; });
    }
}
