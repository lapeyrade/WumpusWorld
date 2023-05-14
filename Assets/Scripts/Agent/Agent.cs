using System;
using System.Collections.Generic;
using Agent.AI;
using UnityEngine;

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

        public void Init(int agentId, Vector2Int newCoords, int nbTotalWumpus)
        {
            name = $"human{agentId}";
            tag = "Human";
            startCoord = newCoords;
            coords = newCoords;
            nbArrow = nbTotalWumpus;
            PastMovements.Push(coords);
            transform.position = GridManager.GetAgentMapOffset(newCoords);

            prefabAgentWorld = Instantiate(Resources.Load("Human"), transform) as GameObject;
            if (prefabAgentWorld is null) return;
            prefabAgentWorld.tag = tag;
            prefabAgentWorld.name = name;
            prefabAgentWorld.transform.position = GridManager.GetWorldMapOffset(newCoords);

            switch (GameManager.Instance.aiType)
            {
                case GameManager.AIType.Prolog:
                    gameObject.AddComponent<AIProlog>();
                    break;
                case GameManager.AIType.BehaviourTree:
                    gameObject.AddComponent<AIBehaviourTree>();
                    break;
                case GameManager.AIType.FiniteStateMachine:
                    gameObject.AddComponent<AIFiniteStateMachine>();
                    break;
                case GameManager.AIType.Gpt:
                    gameObject.AddComponent<AIGpt>();
                    break;
                case GameManager.AIType.Basic:
                default:
                    gameObject.AddComponent<AIBasic>();
                    break;
            }

            foreach (var perso in GameManager.Instance.personalities)
                gameObject.AddComponent(Type.GetType("Ontology." + perso));
        }
    }
}