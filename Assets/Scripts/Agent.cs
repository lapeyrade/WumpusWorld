using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using Random = UnityEngine.Random;

[System.Serializable]
public class Agent : MonoBehaviour
{
    public Vector2Int startCoord;
    public Vector2Int coord;
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
        coord = startCoord;
        nbArrow = nbTotalWumpus;
        PastMovements.Push(coord);
        transform.position = GameManager.Instance.GetAgentMapOffset(newCoord);
        
        prefabAgentWorld = Instantiate(Resources.Load("human"), transform) as GameObject;
        if (prefabAgentWorld is null) return;
        prefabAgentWorld.tag = tag;
        prefabAgentWorld.name = name;
        prefabAgentWorld.transform.position = GameManager.Instance.GetWorldMapOffset(newCoord);
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
            Move(new Vector2Int(coord.x + 1, coord.y)); 
        else if (Input.GetKeyDown("left"))
            Move(new Vector2Int(coord.x - 1, coord.y));
        else if (Input.GetKeyDown("up"))
            Move(new Vector2Int(coord.x, coord.y + 1));
        else if (Input.GetKeyDown("down"))
            Move(new Vector2Int(coord.x, coord.y - 1));
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
            new(coord.x + 1, coord.y), new(coord.x - 1, coord.y),
            new(coord.x, coord.y + 1), new(coord.x, coord.y - 1)
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
            new (coord.x + 1, coord.y), new (coord.x - 1, coord.y),
            new (coord.x, coord.y + 1), new (coord.x, coord.y - 1)
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
        if (startCoord == coord && nbGold == 1)
            GameManager.Instance.SetGameOver($"{name} Won!", false);

        foreach (string element in GameManager.Instance.Map[coord.x, coord.y]
                     .Except(GameManager.Instance.AgentsMap[coord.x, coord.y]).Select(x => x.tag))
        {
            GameManager.Instance.AddToGrids(coord, element);
        }
        
        if (GameManager.Instance.Map[coord.x, coord.y].Exists(e => e.tag is "pit" or "wumpus"))
            GameManager.Instance.SetGameOver($"{name} Lost!", false);

        MakeInferences();
    }

    private void ActionCell()
    {
        // Bump Wall
        if (GameManager.Instance.AgentsMap[coord.x, coord.y].Exists(e => e.tag is "wall"))
            Move(MoveBack());

        // PickUp Gold
        if (GameManager.Instance.AgentsMap[coord.x, coord.y].Exists(e => e.tag is "gold"))
            PickUpGold();

        // Shoot Arrow
        if(nbArrow > 0)
            TryShootingArrow();
    }

    private void Move(Vector2Int newCoord)
    {
        GameManager.Instance.RemoveFromGrids(coord, tag);
        transform.position = GameManager.Instance.GetAgentMapOffset(newCoord);

        if (nbGold > 0)
        {
            prefabGoldMap.transform.position = prefabAgentWorld.transform.position;
            prefabGoldAgent.transform.position = transform.position;
        }

        PastMovements.Push(newCoord);
        coord = newCoord;
        GameManager.Instance.AddToGrids(coord, "visited");
    }

    private Vector2Int MoveBack()
    {
        if (PastMovements.Count <= 1) return coord;
        PastMovements.Pop();
        return PastMovements.Pop();
    }

    private void MakeInferences()
    {
        if (GameManager.Instance.AgentsMap[coord.x, coord.y].Exists(x
                => x.tag is "breeze" or "stench" or "wumpus" or "pit"))
            MarkSideCells("undefined", true);
        else
            MarkSideCells("safe", false);

        CheckCellsDanger("wumpus", "stench");
        CheckCellsDanger("pit", "breeze");
        
        void MarkSideCells(string element, bool checkDanger)
        {
            for (int i = 1; i <= 2; i++)
            {
                if (i > 1)
                    element = "undefined";
                MarkCell(new Vector2Int(coord.x + i, coord.y), element, checkDanger);
                MarkCell(new Vector2Int(coord.x - i, coord.y), element, checkDanger);
                MarkCell(new Vector2Int(coord.x, coord.y + i), element, checkDanger);
                MarkCell(new Vector2Int(coord.x, coord.y - i), element, checkDanger);

                if (i <= 1) continue;
                MarkCell(new Vector2Int(coord.x + (i - 1), coord.y + (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(coord.x + (i - 1), coord.y - (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(coord.x - (i - 1), coord.y + (i - 1)), element, checkDanger);
                MarkCell(new Vector2Int(coord.x - (i - 1), coord.y - (i - 1)), element, checkDanger);
            }
        }

        void MarkCell(Vector2Int cell, string element, bool checkDanger)
        {
            if (!CellInGridLimits(cell)) return;
            if (checkDanger)
                if (GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e =>
                        e.tag is "wumpus" or "wumpusdead" or "pit")) return;
            if (element is "undefined" && GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e =>
                    e.tag is "safe" or "wall" or "visited")) return;

            GameManager.Instance.AddToGrids(cell, element);
        }

        void CheckCellsDanger(string danger, string hint)
        {
            CheckDanger(coord, danger, hint);
            CheckDanger(new Vector2Int(coord.x + 1, coord.y), danger, hint);
            CheckDanger(new Vector2Int(coord.x - 1, coord.y), danger, hint);
            CheckDanger(new Vector2Int(coord.x, coord.y + 1), danger, hint);
            CheckDanger(new Vector2Int(coord.x, coord.y - 1), danger, hint);
            CheckDanger(new Vector2Int(coord.x + 1, coord.y + 1), danger, hint);
            CheckDanger(new Vector2Int(coord.x + 1, coord.y - 1), danger, hint);
            CheckDanger(new Vector2Int(coord.x - 1, coord.y + 1), danger, hint);
            CheckDanger(new Vector2Int(coord.x - 1, coord.y + 1), danger, hint);
        }

        void CheckDanger(Vector2Int cell, string danger, string hint)
        {
            if (!CellInGridLimits(cell) ||
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

        void AddDanger(Vector2Int cell, string danger, string hint)
        {
            if (danger is "wumpus" &&
                GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "wumpusdead"))
                return;
            GameManager.Instance.AddToGrids(cell, danger);
            GameManager.Instance.AddToGrids(cell, "danger");
            AddHint(new Vector2Int(cell.x + 1, cell.y), hint);
            AddHint(new Vector2Int(cell.x - 1, cell.y), hint);
            AddHint(new Vector2Int(cell.x, cell.y + 1), hint);
            AddHint(new Vector2Int(cell.x, cell.y - 1), hint);
        }

        void AddHint(Vector2Int cell, string hint)
        {
            if (GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "wall")) return;
            GameManager.Instance.AddToGrids(cell, hint);
        }

        bool DangerInRightCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
        }

        bool DangerInLeftCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
        }

        bool DangerInUpCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y - 1), danger);
        }

        bool DangerInDownCell(Vector2Int cell, string danger)
        {
            return NoDangerInCell(new Vector2Int(cell.x + 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x - 1, cell.y), danger) &&
                   NoDangerInCell(new Vector2Int(cell.x, cell.y + 1), danger);
        }

        bool NoDangerInCell(Vector2Int cell, string danger)
        {
            if (!CellInGridLimits(cell)) return false;
            if (danger == "wumpus" &&
                GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "wumpusdead"))
                return false;
            return GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.tag is "visited" or "safe" or "start") &&
                   !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.CompareTag(danger));
        }

        bool CellInGridLimits(Vector2Int cell)
        {
            return cell.x >= GameManager.Instance.gridMin.x && cell.x < GameManager.Instance.gridMax.x &&
                   cell.y >= GameManager.Instance.gridMin.y && cell.y < GameManager.Instance.gridMax.y;
        }
    }

    private void PickUpGold()
    {
        nbGold++;
        GameManager.Instance.RemoveFromGrids(coord, "gold");
        GameManager.Instance.AttachGoldToAgent(this);
    }

    private void TryShootingArrow()
    {
        ShootIfWumpusRightOfAgent();
        ShootIfWumpusLeftOfAgent();
        ShootIfWumpusUpOfAgent();
        ShootIfWumpusDownOfAgent();


        void ShootIfWumpusRightOfAgent()
        {
            for (int i = coord.x; i < GameManager.Instance.gridMax.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, coord.y].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(i, coord.y));
            }
        }

        void ShootIfWumpusLeftOfAgent()
        {
            for (int i = GameManager.Instance.gridMin.x; i < coord.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, coord.y].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(i, coord.y));
            }
        }

        void ShootIfWumpusUpOfAgent()
        {
            for (int i = coord.y; i < GameManager.Instance.gridMax.y; i++)
            {
                if (GameManager.Instance.AgentsMap[coord.x, i].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(coord.x, i));
            }
        }

        void ShootIfWumpusDownOfAgent()
        {
            for (int i = GameManager.Instance.gridMin.y; i < coord.y; i++)
            {
                if (GameManager.Instance.AgentsMap[coord.x, i].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(coord.x, i));
            }
        }
    }

    private void ShootWumpus(Vector2Int coordWumpus)
    {
        nbArrow--;
        GameManager.Instance.RemoveFromGrids(coordWumpus, "wumpus");
        GameManager.Instance.RemoveFromGrids(coordWumpus, "danger");
        GameManager.Instance.AddToGrids(coordWumpus, "wumpusdead");
        GameManager.Instance.AddToGrids(coordWumpus, "safe");
    }
}