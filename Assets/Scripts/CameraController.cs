using UnityEngine;

/// <summary>
/// Center the game camera dynamically to the game dimensions
/// </summary>
public class CameraController : MonoBehaviour
{
    public GameObject gridManager;
    private World _world;

    protected void Start()
    {
        _world = gridManager.GetComponent<World>();
    }

    public void AdjustCameraPosition()
    {
        transform.position = new Vector3((_world.gridMax.x - 1) * _world.tileSize / 2, (_world.gridMax.y - 1) * _world.tileSize / 2, -1);

        if (_world.gridMax.x > _world.gridMax.y)
            GetComponent<Camera>().orthographicSize = _world.gridMax.x / 2f;
        else
            GetComponent<Camera>().orthographicSize = _world.gridMax.y / 2f * 1.25f;
    }
}
