using UnityEngine;
using System.Collections.Generic;

public class Human : MonoBehaviour
{
    public string agentName;
    public Coordinates coords;
    public int nbGold = 0;
    public int nbArrow;
    public int nbArrowUsed = 0;

    public int nbWumpus;
    public int nbWumpusDead = 0;

    public GameObject gridManager;

    public Dictionary<string, GameObject>[,] map;

    public Stack<Coordinates> pastMovements = new Stack<Coordinates>();

    protected World world;

    void Awake()
    {
        world = gridManager.GetComponent<World>();
    }

    [SerializeField] public int intelligence = 6;
    [SerializeField] public int strength = 5;
    [SerializeField] public int dexterity = 5;

    // public enum pers { determinist, stochastic, hunter, pacifist, explorer, greedy, nonmaterialistic };

    // [SerializeField] public pers[] personality = new pers[] { pers.determinist };

    public void initAgent(Coordinates startCoords, string name, int nbTotalWumpus, Coordinates gridMax)
    {
        coords = startCoords;
        agentName = name;
        nbWumpus = nbTotalWumpus;
        nbArrow = nbTotalWumpus;
        map = new Dictionary<string, GameObject>[gridMax.col, gridMax.row];
    }

    public void Move(Coordinates newCoords)
    {
        pastMovements.Push(newCoords);
        world.RemoveFromGrids(coords.col, coords.row, agentName, true, true);
        coords = newCoords;
        world.AddToGrids(coords.col, coords.row, agentName, true, true);
        world.AddToGrids(coords.col, coords.row, "visited", true, false);
    }

    // public List<string> getAgentPersonalities()
    // {
    //     List<string> listPersonalies = new List<string>();
    //     foreach (pers p in personality)
    //     {
    //         listPersonalies.Add(p.ToString());
    //     }
    //     return listPersonalies;
    // }

    public void MoveBack()
    {
        if (pastMovements.Count > 1)
        {
            pastMovements.Pop();
            Move(pastMovements.Pop());
        }
    }

    public void TakeGold()
    {
        world.RemoveFromGrids(coords.col, coords.row, "gold", true, true);
        nbGold += 1;
    }
    public void HitWall()
    {
        world.RemoveFromGrids(coords.col, coords.row, agentName, true, true);
        pastMovements.Pop();
        coords = pastMovements.Peek();
        world.AddToGrids(coords.col, coords.row, agentName, true, true);
    }
}