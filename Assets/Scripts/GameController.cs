using UnityEngine;

public class GameController : MonoBehaviour
{
    [Range(0.0f, 1.0f)][SerializeField] public float timerInterval = 0.016f;
    private float _timer;
    
    protected void Start()
    {
        foreach (Human agent in GameManager.Instance.agents)
        {
            agent.PlayTurn();
        }
    }

    protected void Update()
    {
        if (GameManager.Instance.isModeAuto && _timer < timerInterval)
            _timer += Time.deltaTime;
        else
        {
            PlayTurn();
            _timer = 0.0f;
        }
    }

    private void PlayTurn()
    {
        if ((!Input.anyKeyDown && !GameManager.Instance.isModeAuto) || GameManager.Instance.isGameOver) return;
        
        if (Input.GetKeyDown("escape"))
            GameManager.Instance.SetGameOver("Exit Game!", true);
        else if (Input.GetKeyDown("backspace"))
        {
            ScreenCapture.CaptureScreenshot("Screenshots/screenshot " +
                                            System.DateTime.Now.ToString("MM-dd-yy (HH-mm-ss)") + ".png");
            Debug.Log("Screenshot saved!");
        }
        else if (Input.GetKeyDown("return") || Input.GetKeyDown("space") || Input.GetKeyDown("right") ||
                 Input.GetKeyDown("left") || Input.GetKeyDown("up") || Input.GetKeyDown("down") ||
                 GameManager.Instance.isModeAuto)
        {
            foreach (Human agent in GameManager.Instance.agents)
            {
                agent.PlayTurn();
            }
        }
    }
}