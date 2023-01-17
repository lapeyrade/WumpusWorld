using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using Random = UnityEngine.Random;

[System.Serializable]
public class Agent : MonoBehaviour
{
    public Vector2Int startCoord;
    public Vector2Int coords;
    public int nbGold;
    public int nbArrow;

    public GameObject prefabAgentWorld;
    public GameObject prefabGoldAgent;
    public GameObject prefabGoldMap;

    public Stack<Vector2Int> PastMovements = new();

    [SerializeField] public int intelligence = 3;
    [SerializeField] public int strength = 5;
    [SerializeField] public int dexterity = 7;

    [SerializeField] public List<string> personalities;
    
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
    }


    public void PlayTurn()
    {
        MoveCell();
        SenseCell();
        ActionCell();
    }
    
    private void MoveCell()
    {
        if (Input.GetKeyDown("right"))
            Move(new Vector2Int(coords.x + 1, coords.y)); 
        else if (Input.GetKeyDown("left"))
            Move(new Vector2Int(coords.x - 1, coords.y));
        else if (Input.GetKeyDown("up"))
            Move(new Vector2Int(coords.x, coords.y + 1));
        else if (Input.GetKeyDown("down"))
            Move(new Vector2Int(coords.x, coords.y - 1));
        else if (Input.GetKeyDown("space") || GameManager.Instance.isModeAuto) // IA
            Move(SelectNextMove());
        else if (Input.GetKeyDown("return")) // IA Random
            Move(SelectRandomMove());
    }

    private Vector2Int SelectNextMove()
    {
        if (nbGold > 0)
            return MoveBack();
        
        var moves = new List<Vector2Int>
        {
            new(coords.x + 1, coords.y), new(coords.x - 1, coords.y),
            new(coords.x, coords.y + 1), new(coords.x, coords.y - 1)
        };
        
        foreach (var move in moves.Where(SafeCellUnexplored))
        {
            return move;
        }
        return MoveBack();
    }

    private Vector2Int SelectRandomMove()
    {
        if (nbGold > 0)
            return MoveBack();
        
        var randomMoves = new List<Vector2Int>
        {
            new (coords.x + 1, coords.y), new (coords.x - 1, coords.y),
            new (coords.x, coords.y + 1), new (coords.x, coords.y - 1)
        }.OrderBy(_ => Random.value);

        foreach (var move in randomMoves)
        {
            if (SafeCellUnexplored(move)) return move;
        }
        return MoveBack();
    }

    private bool SafeCellUnexplored(Vector2Int cell)
    {
        return GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "safe") &&
               !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "visited");
    }

    private void SenseCell()
    {
        if (startCoord == coords && nbGold == 1)
            GameManager.Instance.SetGameOver($"{name} Won!", false);

        foreach (string element in GameManager.Instance.Map[coords.x, coords.y]
                     .Except(GameManager.Instance.AgentsMap[coords.x, coords.y]).Select(x => x.tag))
        {
            GridManager.AddToGrids(coords, element);
        }
        
        if (GameManager.Instance.Map[coords.x, coords.y].Exists(e => e.tag is "pit" or "wumpus"))
            GameManager.Instance.SetGameOver($"{name} Lost!", false);

        MakeInferences();
    }

    private void ActionCell()
    {
        // Bump Wall
        if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.tag is "wall"))
            Move(MoveBack());

        // PickUp Gold
        if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(e => e.tag is "gold"))
            PickUpGold();

        // Shoot Arrow
        if(nbArrow > 0)
            TryShootingArrow();
    }

    private void Move(Vector2Int newCoord)
    {
        GridManager.RemoveFromGrids(coords, tag);
        transform.position = GridManager.GetAgentMapOffset(newCoord);

        if (nbGold > 0)
        {
            prefabGoldMap.transform.position = prefabAgentWorld.transform.position;
            prefabGoldAgent.transform.position = transform.position;
        }

        PastMovements.Push(newCoord);
        coords = newCoord;
        GridManager.AddToGrids(coords, "visited");
    }

    private Vector2Int MoveBack()
    {
        if (PastMovements.Count <= 1) return coords;
        PastMovements.Pop();
        return PastMovements.Pop();
    }

    private void MakeInferences()
    {
        if (GameManager.Instance.AgentsMap[coords.x, coords.y].Exists(x
                => x.tag is "breeze" or "stench" or "wumpus" or "pit"))
            MarkSideCells("undefined", true);
        else
            MarkSideCells("safe", false);

        CheckCellsDanger("wumpus", "stench");
        CheckCellsDanger("pit", "breeze");
    }

    void MarkSideCells(string element, bool checkDanger)
    {
        for (int i = 1; i <= 2; i++)
        {
            if (i > 1)
                element = "undefined";
            MarkCell(new Vector2Int(coords.x + i, coords.y), element, checkDanger);
            MarkCell(new Vector2Int(coords.x - i, coords.y), element, checkDanger);
            MarkCell(new Vector2Int(coords.x, coords.y + i), element, checkDanger);
            MarkCell(new Vector2Int(coords.x, coords.y - i), element, checkDanger);

            if (i <= 1) continue;
            MarkCell(new Vector2Int(coords.x + (i - 1), coords.y + (i - 1)), element, checkDanger);
            MarkCell(new Vector2Int(coords.x + (i - 1), coords.y - (i - 1)), element, checkDanger);
            MarkCell(new Vector2Int(coords.x - (i - 1), coords.y + (i - 1)), element, checkDanger);
            MarkCell(new Vector2Int(coords.x - (i - 1), coords.y - (i - 1)), element, checkDanger);
        }
    }
    
    private void MarkCell(Vector2Int cell, string element, bool checkDanger)
    {
        if (!GridManager.CellInGridLimits(cell)) return;
        if (checkDanger && GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e =>
                    e.tag is "wumpus" or "wumpusdead" or "pit")) return;
        if (element is "undefined" && GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e =>
                e.tag is "safe" or "wall" or "visited")) return;

        GridManager.AddToGrids(cell, element);
    }

    private void CheckCellsDanger(string danger, string hint)
    {
        CheckDanger(coords, danger, hint);
        CheckDanger(new Vector2Int(coords.x + 1, coords.y), danger, hint);
        CheckDanger(new Vector2Int(coords.x - 1, coords.y), danger, hint);
        CheckDanger(new Vector2Int(coords.x, coords.y + 1), danger, hint);
        CheckDanger(new Vector2Int(coords.x, coords.y - 1), danger, hint);
        CheckDanger(new Vector2Int(coords.x + 1, coords.y + 1), danger, hint);
        CheckDanger(new Vector2Int(coords.x + 1, coords.y - 1), danger, hint);
        CheckDanger(new Vector2Int(coords.x - 1, coords.y + 1), danger, hint);
        CheckDanger(new Vector2Int(coords.x - 1, coords.y + 1), danger, hint);
    }

    private void CheckDanger(Vector2Int cell, string danger, string hint)
    {
        if (!GridManager.CellInGridLimits(cell) ||
            !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.CompareTag(hint))) return;
        if (DangerInRightCell(cell, danger))
            AddDanger(new Vector2Int(cell.x + 1, cell.y), danger, hint);
        if (DangerInLeftCell(cell, danger))
            AddDanger(new Vector2Int(cell.x - 1, cell.y), danger, hint);
        if (DangerInUpCell(cell, danger))
            AddDanger(new Vector2Int(cell.x, cell.y + 1), danger, hint);
        if (DangerInDownCell(cell, danger))
            AddDanger(new Vector2Int(cell.x, cell.y - 1), danger, hint);
    }

    private void AddDanger(Vector2Int cell, string danger, string hint)
    {
        if (danger is "wumpus" &&
            GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "wumpusdead"))
            return;
        GridManager.AddToGrids(cell, danger);
        GridManager.AddToGrids(cell, "danger");
        AddHint(new Vector2Int(cell.x + 1, cell.y), hint);
        AddHint(new Vector2Int(cell.x - 1, cell.y), hint);
        AddHint(new Vector2Int(cell.x, cell.y + 1), hint);
        AddHint(new Vector2Int(cell.x, cell.y - 1), hint);
    }

    private void AddHint(Vector2Int cell, string hint)
    {
        if (GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "wall")) return;
        GridManager.AddToGrids(cell, hint);
    }

    private bool DangerInRightCell(Vector2Int cell, string danger)
    {
        return NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
               NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger) &&
               NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
    }

    private bool DangerInLeftCell(Vector2Int cell, string danger)
    {
        return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
               NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger) &&
               NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
    }

    private bool DangerInUpCell(Vector2Int cell, string danger)
    {
        return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
               NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
               NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
    }

    private bool DangerInDownCell(Vector2Int cell, string danger)
    {
        return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
               NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
               NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger);
    }

    private bool NoDangerInCell(Vector2Int cell, string danger)
    {
        if (!GridManager.CellInGridLimits(cell)) return false;
        if (danger == "wumpus" &&
            GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "wumpusdead"))
            return false;
        return GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "visited" or "safe" or "start") &&
               !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.CompareTag(danger));
    }

    private void PickUpGold()
    {
        nbGold++;
        GridManager.RemoveFromGrids(coords, "gold");
        GridManager.AttachGoldToAgent(this);
    }

    private void TryShootingArrow()
    {
        ShootIfWumpusRightOfAgent();
        ShootIfWumpusLeftOfAgent();
        ShootIfWumpusUpOfAgent();
        ShootIfWumpusDownOfAgent();


        void ShootIfWumpusRightOfAgent()
        {
            for (int i = coords.x; i < GameManager.Instance.gridMax.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, coords.y].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(i, coords.y));
            }
        }

        void ShootIfWumpusLeftOfAgent()
        {
            for (int i = GameManager.Instance.gridMin.x; i < coords.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, coords.y].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(i, coords.y));
            }
        }

        void ShootIfWumpusUpOfAgent()
        {
            for (int i = coords.y; i < GameManager.Instance.gridMax.y; i++)
            {
                if (GameManager.Instance.AgentsMap[coords.x, i].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(coords.x, i));
            }
        }

        void ShootIfWumpusDownOfAgent()
        {
            for (int i = GameManager.Instance.gridMin.y; i < coords.y; i++)
            {
                if (GameManager.Instance.AgentsMap[coords.x, i].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(coords.x, i));
            }
        }
    }

    private void ShootWumpus(Vector2Int coordWumpus)
    {
        nbArrow--;
        GridManager.RemoveFromGrids(coordWumpus, "wumpus");
        GridManager.RemoveFromGrids(coordWumpus, "danger");
        GridManager.AddToGrids(coordWumpus, "wumpusdead");
        GridManager.AddToGrids(coordWumpus, "safe");
    }
}