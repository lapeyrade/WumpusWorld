using System.Collections.Generic;
using System.Linq;
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
        public List<GameManager.Personalities> personalities;
        
        public enum Objectives { Wealth, Abstinence, Safety, Fight, Explore, Unconstrained }
        public List<Objectives> objectives = new ();

        public GameObject prefabAgentWorld;
        public GameObject prefabGoldAgent;
        public GameObject prefabGoldMap;

        public readonly Stack<Vector2Int> PastMovements = new();

        public void Init(int agentId, Vector2Int newCoord, int nbTotalWumpus)
        {
            name = $"agent{agentId}";
            tag = "human";
            startCoord = newCoord;
            coords = startCoord;
            nbArrow = nbTotalWumpus;
            PastMovements.Push(coords);
            transform.position = GridManager.GetAgentMapOffset(newCoord);

            prefabAgentWorld = Instantiate(Resources.Load("human"), transform) as GameObject;
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
                
            personalities = GameManager.Instance.personalities;
        }

        public void MoveCell()
        {
            if (Input.GetKeyDown("right"))
                GetComponent<Move>().MoveAgent(new Vector2Int(coords.x + 1, coords.y));
            else if (Input.GetKeyDown("left"))
                GetComponent<Move>().MoveAgent(new Vector2Int(coords.x - 1, coords.y));
            else if (Input.GetKeyDown("up"))
                GetComponent<Move>().MoveAgent(new Vector2Int(coords.x, coords.y + 1));
            else if (Input.GetKeyDown("down"))
                GetComponent<Move>().MoveAgent(new Vector2Int(coords.x, coords.y - 1));
            else if (Input.GetKeyDown("space") || GameManager.Instance.isModeAuto) // IA
                GetComponent<Move>().MoveAgent(GetComponent<Move>().SelectNextMove());
            else if (Input.GetKeyDown("return")) // IA Random
                GetComponent<Move>().MoveAgent(GetComponent<Move>().SelectRandomMove());
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

            if (!GameManager.Instance.Map[coords.x, coords.y].Exists(e => e.tag is "pit" or "wumpus"))
                GetComponent<Sense>().MakeInferences();
            else GameManager.Instance.SetGameOver($"{name} Lost!", false);
        }

        public void ActionCell()
        {
            if(GameManager.Instance.isGameOver) return;
            
            if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.tag is "wall"))
                GetComponent<Move>().BumpWall();
            
            if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.tag is "gold"))
                GetComponent<Action>().PickUpGold();
            
            GetComponent<Action>().TryShootingArrow();
        }

        public void GenerateObjective()
        {
            if (GetComponent<Agent>().personalities.Contains(GameManager.Personalities.Cupid) &&
                GameManager.Instance.AgentsMap[GetComponent<Agent>().coords.x, GetComponent<Agent>().coords.y]
                    .Exists(e => e.tag is "valuable_item"))
            {
                Debug.Log("Objective: Wealthy");
                GetComponent<Agent>().objectives.Add(Agent.Objectives.Wealth);
            }
        }
    }
}