using System.Collections.Generic;
using UnityEngine;
using Random = UnityEngine.Random;

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
                GridManager.AddToGrids(new Vector2Int(i, j), "Cell");
            }
        }
    }

    private static void GenerateWall()
    {
        for (var i = GameManager.Instance.gridMin.y; i < GameManager.Instance.gridMax.y; i++) // Right
            GridManager.AddToGrids(new Vector2Int(GameManager.Instance.gridMax.x - 1, i), "Wall");

        for (var i = GameManager.Instance.gridMin.y; i < GameManager.Instance.gridMax.y; i++) // Left
            GridManager.AddToGrids(new Vector2Int(GameManager.Instance.gridMin.x, i), "Wall");

        for (var i = GameManager.Instance.gridMin.y + 1; i < GameManager.Instance.gridMax.x - 1; i++) // Top
            GridManager.AddToGrids(new Vector2Int(i, GameManager.Instance.gridMax.y - 1), "Wall");

        for (var i = GameManager.Instance.gridMin.y + 1; i < GameManager.Instance.gridMax.x - 1; i++) // Bottom
            GridManager.AddToGrids(new Vector2Int(i, GameManager.Instance.gridMin.y), "Wall");
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
            } while (GameManager.Instance.Map[coord.x, coord.y].Exists(x => x.tag is "StartCell" or "Wall"));
            
            if (Instantiate(Resources.Load("Human"), transform) is not GameObject agent) continue;
            agent.GetComponent<Agent.Agent>().Init(i, coord, GameManager.Instance.nbWumpus);

            GridManager.AddToGrids(coord, "StartCell");
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
            } while (GameManager.Instance.Map[coord.x, coord.y].Exists(x => x.tag is "StartCell" or "Wall" or "Gold"));

            GridManager.AddToGrids(coord, "Gold");
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
                         x.tag is "StartCell" or "Wall" or "Gold" or "Wumpus"));

            GridManager.AddToGrids(coord, "Wumpus");
            GenerateAroundCell(coord, "Stench");
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
                         x.tag is "StartCell" or "Wall" or "Gold" or "Wumpus" or "Pit"));

            GridManager.AddToGrids(coord, "Pit");
            GenerateAroundCell(coord, "Breeze");
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
                !GameManager.Instance.Map[coords.x, coords.y].Exists(x => x.tag is "Wall"))
                GridManager.AddToGrids(coords, elem);
        }
    }
}
