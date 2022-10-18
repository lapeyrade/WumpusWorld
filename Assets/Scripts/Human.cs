using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class Human
{
    public string agentName;
    public string id;
    public Vector2Int startCoord;
    public Vector2Int coord;
    public int nbGold;
    public int nbArrow;
    
    public GameObject prefabAgentMap;
    public GameObject prefabWorldMap;

    public Stack<Vector2Int> PastMovements = new();

    [SerializeField] public int intelligence = 3;
    [SerializeField] public int strength = 5;
    [SerializeField] public int dexterity = 7;

    [SerializeField] public List<string> personalities;


    public Human(int agentId, string name, Vector2Int newCoord, int nbTotalWumpus)
    {
        id = $"human{agentId.ToString()}";
        agentName = name;
        startCoord = newCoord;
        coord = startCoord;
        nbArrow = nbTotalWumpus;
    }

    public void Move(Vector2Int newCoord)
    {
        PastMovements.Push(newCoord);
        coord = newCoord;
    }

    public Vector2Int MoveBack()
    {
        if (PastMovements.Count <= 1) return coord;
        
        PastMovements.Pop();
        return PastMovements.Pop();
    }
}