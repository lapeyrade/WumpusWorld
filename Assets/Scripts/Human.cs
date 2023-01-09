using System.Collections.Generic;
using System.Linq;
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
    public GameObject prefabGoldAgent;
    public GameObject prefabGoldMap;

    public Stack<Vector2Int> PastMovements = new();

    [SerializeField] public int intelligence = 3;
    [SerializeField] public int strength = 5;
    [SerializeField] public int dexterity = 7;

    [SerializeField] public List<string> personalities;

    public Human(int agentId, string name, Vector2Int newCoord, int nbTotalWumpus)
    {
        id = $"human{agentId}";
        agentName = name;
        startCoord = newCoord;
        coord = startCoord;
        nbArrow = nbTotalWumpus;
        PastMovements.Push(coord);
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
        return GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.Item1 is "safe") &&
               !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.Item1 is "visited");
    }

    private void SenseCell()
    {
        if (startCoord == coord && nbGold == 1)
            GameManager.Instance.SetGameOver($"{id} Won!", false);

        foreach (string element in GameManager.Instance.Map[coord.x, coord.y]
                     .Except(GameManager.Instance.AgentsMap[coord.x, coord.y]).Select(x => x.Item1))
        {
            GameManager.Instance.AddToGrids(coord, element, true, false);
        }
        
        if (GameManager.Instance.Map[coord.x, coord.y].Exists(e => e.Item1 is "pit" or "wumpus"))
        {
            GameManager.Instance.AddToGrids(coord, "danger", true, false);
            GameManager.Instance.SetGameOver($"{id} Lost!", false);
        }
        
        MakeInferences();
    }

    private void ActionCell()
    {
        // Bump Wall
        if (GameManager.Instance.AgentsMap[coord.x, coord.y].Exists(e => e.Item1 is "wall"))
            Move(MoveBack());

        // PickUp Gold
        if (GameManager.Instance.AgentsMap[coord.x, coord.y].Exists(e => e.Item1 is "gold"))
            PickUpGold();

        // Shoot Arrow
        if(nbArrow > 0)
            TryShootingArrow();
    }

    private void Move(Vector2Int newCoord)
    {
        GameManager.Instance.RemoveFromGrids(coord, agentName, true, true);

        if (!GameManager.Instance.Map[newCoord.x, newCoord.y].Exists(x => x.Item1 == agentName))
            GameManager.Instance.Map[newCoord.x, newCoord.y].Add((agentName, null));
        prefabWorldMap.transform.position = GameManager.Instance.GetWorldMapOffset(newCoord);

        if (!GameManager.Instance.AgentsMap[newCoord.x, newCoord.y].Exists(x => x.Item1 == agentName))
            GameManager.Instance.AgentsMap[newCoord.x, newCoord.y].Add((agentName, null));
        prefabAgentMap.transform.position = GameManager.Instance.GetAgentMapOffset(newCoord);

        if (nbGold > 0)
        {
            prefabGoldMap.transform.position = prefabWorldMap.transform.position;
            prefabGoldAgent.transform.position = prefabAgentMap.transform.position;
        }

        PastMovements.Push(newCoord);
        coord = newCoord;
        GameManager.Instance.AddToGrids(coord, "visited", true, false);
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
                => x.Item1 is "breeze" or "stench" or "wumpus" or "pit"))
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
                        e.Item1 is "wumpus" or "wumpusdead" or "pit"))
                    return;
            if (element is "undefined" && GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e =>
                    e.Item1 is "safe")) return;

            GameManager.Instance.AddToGrids(cell, element, true, false);
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
                !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.Item1 == hint)) return;
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
                GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.Item1 == "wumpusdead"))
                return;
            GameManager.Instance.AddToGrids(cell, danger, true, false);
            GameManager.Instance.AddToGrids(cell, "danger", true, false);
            AddHint(new Vector2Int(cell.x + 1, cell.y), hint);
            AddHint(new Vector2Int(cell.x - 1, cell.y), hint);
            AddHint(new Vector2Int(cell.x, cell.y + 1), hint);
            AddHint(new Vector2Int(cell.x, cell.y - 1), hint);
        }

        void AddHint(Vector2Int cell, string hint)
        {
            if (GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.Item1 == "wall")) return;
            GameManager.Instance.AddToGrids(cell, hint, true, false);
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
                GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.Item1 == "wumpusdead"))
                return false;
            return GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.Item1 is "visited" or "safe") &&
                   !GameManager.Instance.AgentsMap[cell.x, cell.y].Exists(e => e.Item1 == danger);
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
        GameManager.Instance.RemoveFromGrids(coord, "gold", true, true);
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
                if (GameManager.Instance.AgentsMap[i, coord.y].Exists(e => e.Item1 is "wumpus"))
                    ShootWumpus(new Vector2Int(i, coord.y));
            }
        }

        void ShootIfWumpusLeftOfAgent()
        {
            for (int i = GameManager.Instance.gridMin.x; i < coord.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, coord.y].Exists(e => e.Item1 is "wumpus"))
                    ShootWumpus(new Vector2Int(i, coord.y));
            }
        }

        void ShootIfWumpusUpOfAgent()
        {
            for (int i = coord.y; i < GameManager.Instance.gridMax.y; i++)
            {
                if (GameManager.Instance.AgentsMap[coord.x, i].Exists(e => e.Item1 is "wumpus"))
                    ShootWumpus(new Vector2Int(coord.x, i));
            }
        }

        void ShootIfWumpusDownOfAgent()
        {
            for (int i = GameManager.Instance.gridMin.y; i < coord.y; i++)
            {
                if (GameManager.Instance.AgentsMap[coord.x, i].Exists(e => e.Item1 is "wumpus"))
                    ShootWumpus(new Vector2Int(coord.x, i));
            }
        }
    }

    private void ShootWumpus(Vector2Int coordWumpus)
    {
        nbArrow--;
        GameManager.Instance.RemoveFromGrids(coordWumpus, "wumpus", true, true);
        GameManager.Instance.RemoveFromGrids(coordWumpus, "danger", true, false);
        GameManager.Instance.AddToGrids(coordWumpus, "wumpusdead", true, true);
        GameManager.Instance.AddToGrids(coordWumpus, "safe", true, false);
    }
}