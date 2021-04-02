using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CameraController : MonoBehaviour
{
    public GameObject gridManager;

    void Start()
    {
        GameController gameController = gridManager.GetComponent<GameController>();
        Coordinates gridMax = gameController.gridMax;
        float tileSize = gameController.tileSize;

        transform.position = new Vector3(((gridMax.col - 1) * tileSize) / 2, (((gridMax.row - 1) * tileSize)) / 2, -1);

        if (gridMax.col > gridMax.row)
            GetComponent<Camera>().orthographicSize = gridMax.col / 2 + 1f;
        else
            GetComponent<Camera>().orthographicSize = gridMax.row / 2 + 1f;
    }
}
