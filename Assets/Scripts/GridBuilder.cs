using System.Collections.Generic;
using UnityEngine;
using Random = UnityEngine.Random;

// GridBuilder class is responsible for generating the game grid with all elements.
public class GridBuilder : MonoBehaviour
{
    // Build the game grid with all elements.
    public void BuildGrid()
    {
        ValidateGridSize(); // Check if the grid size is valid.
        GenerateCell(); // Generate cells in the grid.
        GenerateWall(); // Generate walls around the grid.
        GenerateElement("Human", GameManager.Instance.nbAgent, new List<string> { "StartCell", "Wall" }, false); // Generate human agents on the grid.
        GenerateElement("Gold", GameManager.Instance.nbGold, new List<string> { "StartCell", "Wall", "Gold" }); // Generate gold on the grid.
        GenerateElement("Wumpus", GameManager.Instance.nbWumpus, new List<string> { "StartCell", "Wall", "Gold", "Wumpus" }, true, "Stench"); // Generate Wumpus on the grid.
        GenerateElement("Pit", GameManager.Instance.nbPit, new List<string> { "StartCell", "Wall", "Gold", "Wumpus", "Pit" }, true, "Breeze"); // Generate pits on the grid.
    }

    // Validate if the grid size is enough to contain all elements.
    private void ValidateGridSize()
    {
        if (GameManager.Instance.nbPit + GameManager.Instance.nbWumpus + GameManager.Instance.nbGold +
            GameManager.Instance.nbAgent > GameManager.Instance.gridMax.x * GameManager.Instance.gridMax.y)
        {
            Debug.LogError("Map too small, can't contain all the elements.");
            Application.Quit();
            UnityEditor.EditorApplication.isPlaying = false;
        }
    }

    // Generate cells in the grid.
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

    // Generate walls around the grid.
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

    // Generate the specified element on the grid.
    private void GenerateElement(string element, int count, List<string> occupiedTags, bool generateAround = false, string aroundElement = "")
    {
        for (int i = 0; i < count; i++)
        {
            Vector2Int coords;

            do
            {
                coords = new Vector2Int(Random.Range(GameManager.Instance.gridMin.x + 1, GameManager.Instance.gridMax.x - 1),
                    Random.Range(GameManager.Instance.gridMin.y + 1, GameManager.Instance.gridMax.y - 1));
            } while (GameManager.Instance.Map[coords.x, coords.y].Exists(x => occupiedTags.Contains(x.tag)));

            // Instantiate and initialize human agents on the grid.
            if (element == "Human")
            {
                if (Instantiate(Resources.Load("Human"), transform) is not GameObject agent) continue;
                agent.GetComponent<Agent.Agent>().Init(i, coords, GameManager.Instance.nbWumpus);

                GridManager.AddToGrids(coords, "StartCell");
                GameManager.Instance.agents.Add(agent);
            }

            // Add the element to the grid.
            GridManager.AddToGrids(coords, element);

            // Generate elements around the main element (e.g., Stench around Wumpus, Breeze around Pit).
            if (generateAround)
                GenerateAroundCell(coords, aroundElement);
        }
    }

    // Generate elements around a specific cell.
    private static void GenerateAroundCell(Vector2Int coords, string element)
    {
        Generate(new Vector2Int(coords.x + 1, coords.y), element); // Right cell
        Generate(new Vector2Int(coords.x - 1, coords.y), element); // Left cell
        Generate(new Vector2Int(coords.x, coords.y + 1), element); // Top cell
        Generate(new Vector2Int(coords.x, coords.y - 1), element); // Bottom cell

        // Generate the specified element at the new coordinates.
        void Generate(Vector2Int newCoords, string elem)
        {
            if (!GameManager.Instance.Map[newCoords.x, newCoords.y].Exists(x => x.CompareTag(elem)) &&
                !GameManager.Instance.Map[newCoords.x, newCoords.y].Exists(x => x.tag is "Wall"))
                GridManager.AddToGrids(newCoords, elem);
        }
    }
}

