using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class Human
{
    public string agentName = "";
    public string id = "human";
    public Vector2Int startCoord;
    public Vector2Int coord;
    public int nbGold = 0;
    public int nbArrow;
    public int nbWumpus;

    public GameObject agentMapPrefab;
    public GameObject worldMapPrefab;

    public Stack<Vector2Int> pastMovements = new();

    [SerializeField] public int intelligence = 3;
    [SerializeField] public int strength = 5;
    [SerializeField] public int dexterity = 7;

    [SerializeField] public List<string> personalities;


    public Human(int agentId, string name, Vector2Int newCoord, int nbTotalWumpus)
    {
        id += agentId.ToString();
        agentName = name;
        startCoord = newCoord;
        coord = startCoord;
        nbWumpus = nbTotalWumpus;
        nbArrow = nbTotalWumpus;
    }

    public void Move(Vector2Int newCoord)
    {
        pastMovements.Push(newCoord);
        coord = newCoord;
    }

    public Vector2Int MoveBack()
    {
        if (pastMovements.Count > 1)
        {
            pastMovements.Pop();
            return pastMovements.Pop();
        }
        return coord;
    }
}