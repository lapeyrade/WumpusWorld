/// <summary>
/// Coordinates used for the grid of the game
/// </summary>
public class Coordinates
{
    public int col { get; set; }
    public int row { get; set; }

    public Coordinates(int coordCol, int coordRow)
    {
        col = coordCol;
        row = coordRow;
    }

    public override string ToString()
    {
        return "[" + col.ToString() + ", " + row.ToString() + "], ";
    }
}