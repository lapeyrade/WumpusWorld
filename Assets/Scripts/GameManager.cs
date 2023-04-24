using System;
using System.Collections.Generic;
using Ontology;
using TMPro;
using UnityEngine;
using Random = UnityEngine.Random;

public class GameManager : MonoBehaviour
{
    public static GameManager Instance;
    public bool isGameOver;
    public bool isModeAuto;
    [Range(0.0f, 1.0f)][SerializeField] public float timerInterval = 0.016f;

    public int randomSeed = 1;
    public Vector2Int gridMin = new(0, 0); 
    public Vector2Int gridMax = new(15, 15);
    public float tileSize = 1.05f;
    
    public int nbPit = 4;
    public int nbWumpus = 2;
    public int nbGold = 1;
    public int nbAgent = 2;
    public enum AIType { Prolog, BehaviourTree, Gpt, Basic}
    public AIType aiType = AIType.Basic;
    public enum Personalities { Cupid, Ascetic, Brave, Coward}
    public List<Personalities> personalities = new (){ Personalities.Brave };

    public List<GameObject> agents;
    
    public List<GameObject>[,] Map;
    public List<GameObject>[,] AgentsMap;

    protected void Awake()
    {
        Instance = this;
        Random.InitState(randomSeed);

        Map = new List<GameObject>[gridMax.x, gridMax.y];
        AgentsMap = new List<GameObject>[gridMax.x, gridMax.y];
        agents = new List<GameObject>();
        
        GetComponent<GridBuilder>().BuildGrid();
        GameObject.Find("Main Camera").GetComponent<CameraController>().AdjustCameraPosition();
        gameObject.AddComponent<GameController>();
    }

    public static void UpdateMoveGUI(string message)
    {
        // GameObject.Find("AgentMove").GetComponent<TextMeshProUGUI>().text = "Last Move: " + message;
    }
    
    public static void UpdateActionGUI(string message)
    {
        // GameObject.Find("AgentAction").GetComponent<TextMeshProUGUI>().text = "Last Action: " + message;
    }

    public void SetGameOver(string message, bool exitApp)
    {
        isGameOver = true;
        // GameObject.Find("GameOver").GetComponent<TextMeshProUGUI>().text = message;
        if (!exitApp) return;
        Application.Quit();
        UnityEditor.EditorApplication.isPlaying = false;
    }
}