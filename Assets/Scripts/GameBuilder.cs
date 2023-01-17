using System.Collections.Generic;
using UnityEngine;

public class GameBuilder : MonoBehaviour
{
    public int nbPit = 4;
    public int nbWumpus = 2;
    public int nbGold = 1;
    public int nbAgent = 2;
    
    public void BuildMaps()
    {
        if (nbPit + nbWumpus + nbGold + nbAgent > GameManager.Instance.gridMax.x * GameManager.Instance.gridMax.y)
        {
            Debug.LogError("Map too small, can't contain all the elements.");
            Application.Quit();
            UnityEditor.EditorApplication.isPlaying = false;
        }
    
        GenerateGrid();
        GenerateWall();
        GenerateHuman();
        GenerateGold();
        GenerateWumpus();
        GeneratePit();
    }
    
    private void GenerateGrid()
    {
        for (int i = GameManager.Instance.gridMin.x; i < GameManager.Instance.gridMax.x; i++)
        {
            for (int j = GameManager.Instance.gridMin.y; j < GameManager.Instance.gridMax.y; j++)
            {
                GameManager.Instance.AgentsMap[i, j] = new List<GameObject>();
                GameManager.Instance.Map[i, j] = new List<GameObject>();
                GameManager.Instance.AddToGrids(new Vector2Int(i, j), "cell");
            }
        }
    }

    private void GenerateWall()
    {
        for (int i = GameManager.Instance.gridMin.y; i < GameManager.Instance.gridMax.y; i++) // Right
            GameManager.Instance.AddToGrids(new Vector2Int(GameManager.Instance.gridMax.x - 1, i), "wall");

        for (int i = GameManager.Instance.gridMin.y; i < GameManager.Instance.gridMax.y; i++) // Left
            GameManager.Instance.AddToGrids(new Vector2Int(GameManager.Instance.gridMin.x, i), "wall");

        for (int i = GameManager.Instance.gridMin.y + 1; i < GameManager.Instance.gridMax.x - 1; i++) // Top
            GameManager.Instance.AddToGrids(new Vector2Int(i, GameManager.Instance.gridMax.y - 1), "wall");

        for (int i = GameManager.Instance.gridMin.y + 1; i < GameManager.Instance.gridMax.x - 1; i++) // Bottom
            GameManager.Instance.AddToGrids(new Vector2Int(i, GameManager.Instance.gridMin.y), "wall");
    }

    private void GenerateHuman()
    {
        for (int i = 0; i < nbAgent; i++)
        {
            Vector2Int coord;

            do
            {
                coord = new Vector2Int(Random.Range(GameManager.Instance.gridMin.x + 1, GameManager.Instance.gridMax.x - 1),
                    Random.Range(GameManager.Instance.gridMin.y + 1, GameManager.Instance.gridMax.y - 1));
            } while (GameManager.Instance.Map[coord.x, coord.y].Exists(x => x.tag is "start" or "wall"));
            
            if (Instantiate(Resources.Load("human"), transform) is not GameObject agent) continue;
            agent.GetComponent<Agent>().Init(i, coord, nbWumpus);

            GameManager.Instance.AddToGrids(coord, "start");
            GameManager.Instance.agents.Add(agent);
        }
    }

    private void GenerateGold()
    {
        for (int i = 0; i < nbGold; i++)
        {
            Vector2Int coord;
            do
            {
                coord = new Vector2Int(Random.Range(GameManager.Instance.gridMin.x + 1, GameManager.Instance.gridMax.x - 1),
                    Random.Range(GameManager.Instance.gridMin.y + 1, GameManager.Instance.gridMax.y - 1));
            } while (GameManager.Instance.Map[coord.x, coord.y].Exists(x => x.tag is "start" or "wall" or "gold"));

            GameManager.Instance.AddToGrids(coord, "gold");
        }
    }

    private void GenerateWumpus()
    {
        for (int i = 0; i < nbWumpus; i++)
        {
            Vector2Int coord;
            do
            {
                coord = new Vector2Int(Random.Range(GameManager.Instance.gridMin.x + 1, GameManager.Instance.gridMax.x - 1),
                    Random.Range(GameManager.Instance.gridMin.y + 1, GameManager.Instance.gridMax.y - 1));
            } while (GameManager.Instance.Map[coord.x, coord.y].Exists(x =>
                         x.tag is "start" or "wall" or "gold" or "wumpus"));

            GameManager.Instance.AddToGrids(coord, "wumpus");
            GenerateAroundCell(coord, "stench");
        }
    }

    private void GeneratePit()
    {
        for (int i = 0; i < nbPit; i++)
        {
            Vector2Int coord;
            do
            {
                coord = new Vector2Int(Random.Range(GameManager.Instance.gridMin.x + 1, GameManager.Instance.gridMax.x - 1),
                    Random.Range(GameManager.Instance.gridMin.y + 1, GameManager.Instance.gridMax.y - 1));
            } while (GameManager.Instance.Map[coord.x, coord.y].Exists(x =>
                         x.tag is "start" or "wall" or "gold" or "wumpus" or "pit"));

            GameManager.Instance.AddToGrids(coord, "pit");
            GenerateAroundCell(coord, "breeze");
        }
    }

    private void GenerateAroundCell(Vector2Int coord, string element)
    {
        Generate(new Vector2Int(coord.x + 1, coord.y), element); // Right cell
        Generate(new Vector2Int(coord.x - 1, coord.y), element); // Left cell
        Generate(new Vector2Int(coord.x, coord.y + 1), element); // Top cell
        Generate(new Vector2Int(coord.x, coord.y - 1), element); // Bottom cell

        void Generate(Vector2Int coords, string elem)
        {
            if (!GameManager.Instance.Map[coords.x, coords.y].Exists(x => x.CompareTag(elem)) &&
                !GameManager.Instance.Map[coords.x, coords.y].Exists(x => x.tag is "wall"))
                GameManager.Instance.AddToGrids(coords, elem);
        }
    }
}
