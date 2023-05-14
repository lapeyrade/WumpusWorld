using System.Collections.Generic;
using Prolog;
using UnityEngine;
using Random = UnityEngine.Random;

public class GameManager : MonoBehaviour
{
    // Singleton instance
    public static GameManager Instance;

    // Game status and settings
    public bool isGameOver;
    public bool isModeAuto;
    [Range(0.0f, 1.0f)][SerializeField] public float timerInterval = 0.016f;

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
    public enum AIType { Prolog, BehaviourTree, Gpt, Basic, FiniteStateMachine }
    public AIType aiType = AIType.Basic;
    public enum Personalities { Cupid, Ascetic, Brave, Coward }
    public List<Personalities> personalities = new() { Personalities.Brave };

    // Game objects
    public List<GameObject> agents;
    public List<GameObject>[,] Map;
    public List<GameObject>[,] AgentsMap;

    // Initialize GameManager and its components
    protected void Awake()
    {
        // Set up singleton instance
        Instance = this;

        // Initialize random seed
        Random.InitState(randomSeed);

        // Initialize game object grids
        Map = new List<GameObject>[gridMax.x, gridMax.y];
        AgentsMap = new List<GameObject>[gridMax.x, gridMax.y];

        // Initialize agents list
        agents = new List<GameObject>();

        // Initialize Prolog interface if AI type is Prolog
        if (aiType is AIType.Prolog)
        {
            GetComponent<PrologInterface>().Init();
        }

        // Build grid and adjust camera
        GetComponent<GridBuilder>().BuildGrid();
        GameObject.Find("Main Camera").GetComponent<CameraController>().AdjustCameraPosition();

        // Add GameController component
        gameObject.AddComponent<GameController>();
    }

    // Set game over status and optionally exit the application
    public void SetGameOver(bool exitApp)
    {
        isGameOver = true;

        if (!exitApp) return;

        Application.Quit();
        UnityEditor.EditorApplication.isPlaying = false;
    }

    // Check if given coordinates are within the grid bounds
    public static bool IsWithinGrid(int newX, int newY) =>
        newX >= Instance.gridMin.x && newX < Instance.gridMax.x && newY >= Instance.gridMin.y && newY < Instance.gridMax.y;
}