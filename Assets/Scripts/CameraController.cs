using UnityEngine;

public class CameraController : MonoBehaviour
{
    public GameObject gridManager;

    public void AdjustCameraPosition()
    {
        World world = gridManager.GetComponent<World>();
        Coordinates gridMax = world.gridMax;
        float tileSize = world.tileSize;

        transform.position = new Vector3(((gridMax.col - 1) * tileSize) / 2, (((gridMax.row - 1) * tileSize)) / 2, -1);

        if (gridMax.col > gridMax.row)
            GetComponent<Camera>().orthographicSize = gridMax.col / 2 + 1f;
        else
            GetComponent<Camera>().orthographicSize = gridMax.row / 2 + 1f;
    }
}
