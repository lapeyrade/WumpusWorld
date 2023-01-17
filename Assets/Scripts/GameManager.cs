using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Serialization;

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

    public List<GameObject>[,] Map;
    public List<GameObject>[,] AgentsMap;
    public List<GameObject> agents;

    protected void Awake()
    {
        Instance = this;
        Random.InitState(randomSeed);

        Map = new List<GameObject>[gridMax.x, gridMax.y];
        AgentsMap = new List<GameObject>[gridMax.x, gridMax.y];
        agents = new List<GameObject>();
        
        GetComponent<GridBuilder>().BuildGrid();
        GameObject.Find("Main Camera").GetComponent<CameraController>().AdjustCameraPosition();
    }

    public void SetGameOver(string message, bool exitApp)
    {
        isGameOver = true;
        Debug.Log(message);
        if (!exitApp) return;
        Application.Quit();
        UnityEditor.EditorApplication.isPlaying = false;
    }
}