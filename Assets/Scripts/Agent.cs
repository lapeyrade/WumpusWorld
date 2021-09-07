using UnityEngine;
using System.Collections.Generic;

/// <summary>
/// Contains agent characteristics and actions
/// </summary>

public class Agent : MonoBehaviour
{
    public Coordinates coords;
    public int nbGold = 0;
    public int nbArrow;
    public Dictionary<string, GameObject>[,] map;

    public GameObject gridManager;
    private World world;

    public Stack<Coordinates> pastMovements = new Stack<Coordinates>();

    public GameObject spriteAgent;
    public GameObject spriteAgentWorld;

    void Awake()
    {
        world = gridManager.GetComponent<World>();
    }

    public void InitAgent(Coordinates startCoords, int nbWumpus, Coordinates gridMax)
    {
        spriteAgent = (GameObject)Instantiate(Resources.Load("agent"));
        spriteAgentWorld = (GameObject)Instantiate(Resources.Load("agent"));
        coords = startCoords;
        nbArrow = nbWumpus;
        map = new Dictionary<string, GameObject>[gridMax.col, gridMax.row];
    }

    public void MoveBack()
    {
        if (pastMovements.Count > 1)
        {
            pastMovements.Pop();
            Move(pastMovements.Pop());
        }
    }

    public void Move(Coordinates newCoords)
    {
        pastMovements.Push(newCoords);
        world.RemoveFromGrids(coords.col, coords.row, "agent", true, true);
        coords = newCoords;
        world.AddToGrids(coords.col, coords.row, "agent", true, true);
        world.AddToGrids(coords.col, coords.row, "visited", true, false);
    }

    public void TakeGold()
    {
        world.RemoveFromGrids(coords.col, coords.row, "gold", true, true);
        nbGold += 1;
    }
    
    public void HitWall()
    {
        world.RemoveFromGrids(coords.col, coords.row, "agent", true, true);
        // world.RemoveFromGrids(coords.col, coords.row, "visited", true, false);
        pastMovements.Pop();
        coords = pastMovements.Peek();
        world.AddToGrids(coords.col, coords.row, "agent", true, true);
    }
}