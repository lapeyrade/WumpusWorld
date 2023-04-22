using System.Collections.Generic;
using UnityEngine;

public class GridBuilder : MonoBehaviour
{
    public void BuildGrid()
    {
        if (GameManager.Instance.nbPit + GameManager.Instance.nbWumpus + GameManager.Instance.nbGold +
            GameManager.Instance.nbAgent > GameManager.Instance.gridMax.x * GameManager.Instance.gridMax.y)
        {
            Debug.LogError("Map too small, can't contain all the elements.");
            Application.Quit();
            UnityEditor.EditorApplication.isPlaying = false;
        }
        
        GenerateCell();
        GenerateWall();
        GenerateHuman();
        GenerateGold();
        GenerateWumpus();
        GeneratePit();
    }
    
    private static void GenerateCell()
    {
        for (var i = GameManager.Instance.gridMin.x; i < GameManager.Instance.gridMax.x; i++)
        {
            for (var j = GameManager.Instance.gridMin.y; j < GameManager.Instance.gridMax.y; j++)
            {
                GameManager.Instance.AgentsMap[i, j] = new List<GameObject>();
                GameManager.Instance.Map[i, j] = new List<GameObject>();
                GridManager.AddToGrids(new Vector2Int(i, j), "cell");
            }
        }
    }

    private static void GenerateWall()
    {
        for (var i = GameManager.Instance.gridMin.y; i < GameManager.Instance.gridMax.y; i++) // Right
            GridManager.AddToGrids(new Vector2Int(GameManager.Instance.gridMax.x - 1, i), "wall");

        for (var i = GameManager.Instance.gridMin.y; i < GameManager.Instance.gridMax.y; i++) // Left
            GridManager.AddToGrids(new Vector2Int(GameManager.Instance.gridMin.x, i), "wall");

        for (var i = GameManager.Instance.gridMin.y + 1; i < GameManager.Instance.gridMax.x - 1; i++) // Top
            GridManager.AddToGrids(new Vector2Int(i, GameManager.Instance.gridMax.y - 1), "wall");

        for (var i = GameManager.Instance.gridMin.y + 1; i < GameManager.Instance.gridMax.x - 1; i++) // Bottom
            GridManager.AddToGrids(new Vector2Int(i, GameManager.Instance.gridMin.y), "wall");
    }

    private void GenerateHuman()
    {
        for (var i = 0; i < GameManager.Instance.nbAgent; i++)
        {
            Vector2Int coord;

            do
            {
                coord = new Vector2Int(Random.Range(GameManager.Instance.gridMin.x + 1, GameManager.Instance.gridMax.x - 1),
                    Random.Range(GameManager.Instance.gridMin.y + 1, GameManager.Instance.gridMax.y - 1));
            } while (GameManager.Instance.Map[coord.x, coord.y].Exists(x => x.tag is "start" or "wall"));
            
            if (Instantiate(Resources.Load("human"), transform) is not GameObject agent) continue;
            agent.GetComponent<Agent.Agent>().Init(i, coord, GameManager.Instance.nbWumpus);

            GridManager.AddToGrids(coord, "start");
            GameManager.Instance.agents.Add(agent);
        }
    }

    private static void GenerateGold()
    {
        for (var i = 0; i < GameManager.Instance.nbGold; i++)
        {
            Vector2Int coord;
            do
            {
                coord = new Vector2Int(Random.Range(GameManager.Instance.gridMin.x + 1, GameManager.Instance.gridMax.x - 1),
                    Random.Range(GameManager.Instance.gridMin.y + 1, GameManager.Instance.gridMax.y - 1));
            } while (GameManager.Instance.Map[coord.x, coord.y].Exists(x => x.tag is "start" or "wall" or "gold"));

            GridManager.AddToGrids(coord, "gold");
        }
    }

    private static void GenerateWumpus()
    {
        for (var i = 0; i < GameManager.Instance.nbWumpus; i++)
        {
            Vector2Int coord;
            do
            {
                coord = new Vector2Int(Random.Range(GameManager.Instance.gridMin.x + 1, GameManager.Instance.gridMax.x - 1),
                    Random.Range(GameManager.Instance.gridMin.y + 1, GameManager.Instance.gridMax.y - 1));
            } while (GameManager.Instance.Map[coord.x, coord.y].Exists(x =>
                         x.tag is "start" or "wall" or "gold" or "wumpus"));

            GridManager.AddToGrids(coord, "wumpus");
            GenerateAroundCell(coord, "stench");
        }
    }

    private static void GeneratePit()
    {
        for (var i = 0; i < GameManager.Instance.nbPit; i++)
        {
            Vector2Int coord;
            do
            {
                coord = new Vector2Int(Random.Range(GameManager.Instance.gridMin.x + 1, GameManager.Instance.gridMax.x - 1),
                    Random.Range(GameManager.Instance.gridMin.y + 1, GameManager.Instance.gridMax.y - 1));
            } while (GameManager.Instance.Map[coord.x, coord.y].Exists(x =>
                         x.tag is "start" or "wall" or "gold" or "wumpus" or "pit"));

            GridManager.AddToGrids(coord, "pit");
            GenerateAroundCell(coord, "breeze");
        }
    }

    private static void GenerateAroundCell(Vector2Int coord, string element)
    {
        Generate(new Vector2Int(coord.x + 1, coord.y), element); // Right cell
        Generate(new Vector2Int(coord.x - 1, coord.y), element); // Left cell
        Generate(new Vector2Int(coord.x, coord.y + 1), element); // Top cell
        Generate(new Vector2Int(coord.x, coord.y - 1), element); // Bottom cell

        void Generate(Vector2Int coords, string elem)
        {
            if (!GameManager.Instance.Map[coords.x, coords.y].Exists(x => x.CompareTag(elem)) &&
                !GameManager.Instance.Map[coords.x, coords.y].Exists(x => x.tag is "wall"))
                GridManager.AddToGrids(coords, elem);
        }
    }
}
