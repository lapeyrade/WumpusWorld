using UnityEngine;

/// <summary>
/// Center the game camera dynamically to the game dimensions
/// </summary>
public class CameraController : MonoBehaviour
{
    public GameObject gridManager;
    private World world;

    protected void Start()
    {
        world = gridManager.GetComponent<World>();
    }

    public void AdjustCameraPosition()
    {
        transform.position = new Vector3((world.gridMax.x - 1) * world.tileSize / 2, (world.gridMax.y - 1) * world.tileSize / 2, -1);

        if (world.gridMax.x > world.gridMax.y)
            GetComponent<Camera>().orthographicSize = world.gridMax.x / 2 + 1f;
        else
            GetComponent<Camera>().orthographicSize = world.gridMax.y / 2 + 1f;
    }
}
