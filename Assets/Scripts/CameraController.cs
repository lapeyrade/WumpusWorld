using UnityEngine;

public class CameraController : MonoBehaviour
{
    private void Start()
    {
        transform.position = new Vector3((GameManager.Instance.gridMax.x - 1) * GameManager.Instance.tileSize / 2,
            (GameManager.Instance.gridMax.y - 1) * GameManager.Instance.tileSize / 2, -1);

        if (GameManager.Instance.gridMax.x > GameManager.Instance.gridMax.y)
            GetComponent<Camera>().orthographicSize = GameManager.Instance.gridMax.x / 2f;
        else
            GetComponent<Camera>().orthographicSize = GameManager.Instance.gridMax.y / 2f * 1.25f;
    }
}
