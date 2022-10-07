using UnityEngine;
using System.Collections.Generic;

public class Human : MonoBehaviour
{
    public string name;
    public int id;
    public Coordinates coords;
    public int nbGold = 0;
    public int nbArrow;
    public int nbWumpus;

    public Stack<Coordinates> pastMovements = new Stack<Coordinates>();

    [SerializeField] public int intelligence = 3;
    [SerializeField] public int strength = 7;
    [SerializeField] public int dexterity = 7;

    public Human(int agentId, string agentName, Coordinates startCoords, int nbTotalWumpus)
    {
        id = agentId;
        name = agentName;
        coords = startCoords;
        nbWumpus = nbTotalWumpus;
        nbArrow = nbTotalWumpus;
    }

    public void Move(Coordinates newCoords)
    {
        pastMovements.Push(newCoords);
        coords = newCoords;
    }

    public Coordinates MoveBack()
    {
        if (pastMovements.Count > 1)
        {
            pastMovements.Pop();
            return pastMovements.Pop();
        }
        return coords;
    }

    public void TakeGold()
    {
        Debug.Log("Taking Gold");
        nbGold += 1;
    }

    public void BumpWall()
    {
        Debug.Log("Bumping Wall");
        pastMovements.Pop();
        coords = pastMovements.Peek();
    }
}