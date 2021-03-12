using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CameraController : MonoBehaviour
{
    public GameObject gridManager;

    void Start()
    {
        GameController gameController = gridManager.GetComponent<GameController>();
        Coordinates grid = gameController.grid;
        float tileSize = gameController.tileSize;

        transform.position = new Vector3(((grid.x - 1) * tileSize) / 2, (((grid.y - 1) * tileSize)) / 2, -1);

        if (grid.x > grid.y)
            GetComponent<Camera>().orthographicSize = grid.x / 2 + 1f;
        else
            GetComponent<Camera>().orthographicSize = grid.y / 2 + 1f;
    }
}
