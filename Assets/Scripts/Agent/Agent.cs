using System;
using System.Collections.Generic;
using System.Linq;
using Agent.AI;
using Ontology;
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

        public readonly Stack<Vector2Int> PastMovements = new();

        public void Init(int agentId, Vector2Int newCoord, int nbTotalWumpus)
        {
            name = $"human{agentId}";
            tag = "Human";
            startCoord = newCoord;
            coords = startCoord;
            nbArrow = nbTotalWumpus;
            PastMovements.Push(coords);
            transform.position = GridManager.GetAgentMapOffset(newCoord);

            prefabAgentWorld = Instantiate(Resources.Load("Human"), transform) as GameObject;
            if (prefabAgentWorld is null) return;
            prefabAgentWorld.tag = tag;
            prefabAgentWorld.name = name;
            prefabAgentWorld.transform.position = GridManager.GetWorldMapOffset(newCoord);

            switch (GameManager.Instance.aiType)
            {
                case GameManager.AIType.Prolog:
                    gameObject.AddComponent<AIProlog>();
                    break;
                case GameManager.AIType.BehaviourTree:
                    gameObject.AddComponent<AIBehaviourTree>();
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

        public void MoveCell()
        {
            if (Input.GetKeyDown("right"))
                GetComponent<AgentMove>().MoveAgent(new Vector2Int(coords.x + 1, coords.y));
            else if (Input.GetKeyDown("left"))
                GetComponent<AgentMove>().MoveAgent(new Vector2Int(coords.x - 1, coords.y));
            else if (Input.GetKeyDown("up"))
                GetComponent<AgentMove>().MoveAgent(new Vector2Int(coords.x, coords.y + 1));
            else if (Input.GetKeyDown("down"))
                GetComponent<AgentMove>().MoveAgent(new Vector2Int(coords.x, coords.y - 1));
            else if (Input.GetKeyDown("space") || GameManager.Instance.isModeAuto) // IA
                GetComponent<AgentMove>().MoveAgent(GetComponent<AgentMove>().SelectNextMove());
            else if (Input.GetKeyDown("return")) // IA Random
                GetComponent<AgentMove>().MoveAgent(GetComponent<AgentMove>().SelectRandomMove());
        }

        public void SenseCell()
        {
            if (startCoord == coords && nbGold == 1)
                GameManager.Instance.SetGameOver($"{name} Won!", false);

            foreach (var element in GameManager.Instance.Map[coords.x, coords.y]
                         .Except(GameManager.Instance.AgentsMap[coords.x, coords.y]).Select(x => x.tag))
            {
                GridManager.AddToGrids(coords, element);
            }

            if (!GameManager.Instance.Map[coords.x, coords.y].Exists(e => e.tag is "Pit" or "Wumpus"))
                GetComponent<AgentSense>().MakeInferences();
            else GameManager.Instance.SetGameOver($"{name} Lost!", false);
        }

        public void ActionCell()
        {
            if(GameManager.Instance.isGameOver) return;
            
            if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.tag is "Wall"))
                GetComponent<AgentMove>().BumpWall();
            
            if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.tag is "Gold"))
                GetComponent<AgentAction>().PickUpGold();
            
            GetComponent<AgentAction>().TryShootingArrow();
        }

        public void GenerateObjective()
        {
            if (GetComponent<Cupid>())
            {
                if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.GetComponent<ValuableItem>()))
                    gameObject.AddComponent<Wealth>();
            }
            if (GetComponent<Brave>())
            {
                if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.GetComponent<Monster>()))
                    gameObject.AddComponent<Fight>();
                if (GameManager.Instance.gridMax.x > coords.x + 1 &&
                    GameManager.Instance.AgentsMap[coords.x + 1, coords.y].Exists(e => e.GetComponent<Monster>()))
                    gameObject.AddComponent<Fight>();
                if (GameManager.Instance.gridMin.x < coords.x - 1 &&
                    GameManager.Instance.AgentsMap[coords.x - 1, coords.y].Exists(e => e.GetComponent<Monster>()))
                    gameObject.AddComponent<Fight>();
                if (GameManager.Instance.gridMin.y > coords.y + 1 &&
                    GameManager.Instance.AgentsMap[coords.x, coords.y + 1].Exists(e => e.GetComponent<Monster>()))
                    gameObject.AddComponent<Fight>();
                if (GameManager.Instance.gridMin.y < coords.y - 1 && 
                    GameManager.Instance.AgentsMap[coords.x, coords.y - 1].Exists(e => e.GetComponent<Monster>()))
                    gameObject.AddComponent<Fight>();
            }
            if (GetComponent<Coward>())
            {
                if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.GetComponent<DangerousCell>()))
                    gameObject.AddComponent<Safety>();
                if (GameManager.Instance.gridMax.x > coords.x + 1 &&
                    GameManager.Instance.AgentsMap[coords.x + 1, coords.y].Exists(e => e.GetComponent<DangerousCell>()))
                    gameObject.AddComponent<Safety>();
                if (GameManager.Instance.gridMin.x < coords.x - 1 &&
                    GameManager.Instance.AgentsMap[coords.x - 1, coords.y].Exists(e => e.GetComponent<DangerousCell>()))
                    gameObject.AddComponent<Safety>();
                if (GameManager.Instance.gridMin.y > coords.y + 1 &&
                    GameManager.Instance.AgentsMap[coords.x, coords.y + 1].Exists(e => e.GetComponent<DangerousCell>()))
                    gameObject.AddComponent<Safety>();
                if (GameManager.Instance.gridMin.y < coords.y - 1 && 
                    GameManager.Instance.AgentsMap[coords.x, coords.y - 1].Exists(e => e.GetComponent<DangerousCell>()))
                    gameObject.AddComponent<Safety>();
            }
            if (GetComponent<Ascetic>())
            {
                if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.GetComponent<Item>()))
                    gameObject.AddComponent<Abstinence>();
            }
            if (GetComponent<Personality>())
            {
                if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.GetComponent<Obstacle>()))
                    gameObject.AddComponent<Unconstrained>();
            }

            if (GetComponent<Personality>())
            {
                if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.GetComponent<SafeCell>()) ||
                    GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.GetComponent<VisitedCell>()) ||
                    GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.GetComponent<StartCell>()))
                    gameObject.AddComponent<Explore>();
            }
        }

        public void GenerateAction()
        {
            if (GetComponent<Wealth>() && GetComponent<Cupid>())
                gameObject.AddComponent<PickUp>();
            if (GetComponent<Abstinence>() && GetComponent<Ascetic>())
                gameObject.AddComponent<Drop>();
            if (GetComponent<Safety>() && GetComponent<Coward>())
                gameObject.AddComponent<MoveBack>();
            if (GetComponent<Safety>() && GetComponent<Brave>())
                gameObject.AddComponent<Attack>();
            if (GetComponent<Fight>() && GetComponent<Brave>())
                gameObject.AddComponent<Attack>();
            if (GetComponent<Explore>() && GetComponent<Personality>())
                gameObject.AddComponent<Move>();
            if (GetComponent<Unconstrained>() && GetComponent<Personality>())
                gameObject.AddComponent<BumpWall>();
        }
    }
}