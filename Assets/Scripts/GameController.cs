using Agent.AI;
using UnityEngine;

public class GameController : MonoBehaviour
{
    private float _timer;

    // Called before the first frame update
    protected void Start()
    {
        // Initialize agents' first turns
        foreach (var agent in GameManager.Instance.agents)
            agent.GetComponent<AIBasic>().FirstTurn();
    }

    // Called once per frame
    protected void Update()
    {
        // Increment timer if in auto mode, otherwise reset it and call PlayTurn()
        if (GameManager.Instance.isModeAuto && _timer < GameManager.Instance.timerInterval)
            _timer += Time.deltaTime;
        else
        {
            PlayTurn();
            _timer = 0.0f;
        }
    }

    // Execute agents' turns based on input
    private static void PlayTurn()
    {
        // Return if no relevant input or game is over
        if ((!Input.anyKeyDown && !GameManager.Instance.isModeAuto) || GameManager.Instance.isGameOver) return;

        // Check for specific key inputs and perform corresponding actions
        if (Input.GetKeyDown("escape"))
            GameManager.Instance.SetGameOver(true);
        else if (Input.GetKeyDown("backspace"))
        {
            ScreenCapture.CaptureScreenshot("Screenshots/screenshot " +
                                            System.DateTime.Now.ToString("MM-dd-yy (HH-mm-ss)") + ".png");
            Debug.Log("Screenshot saved!");
        }
        // Play turn if specific keys are pressed or game is in auto mode
        else if (Input.GetKeyDown("return") || Input.GetKeyDown("space") || Input.GetKeyDown("right") ||
                 Input.GetKeyDown("left") || Input.GetKeyDown("up") || Input.GetKeyDown("down") ||
                 GameManager.Instance.isModeAuto)
        {
            // Execute agents' turns and measure the execution time
            foreach (var agent in GameManager.Instance.agents)
            {
                var watch = System.Diagnostics.Stopwatch.StartNew();
                agent.GetComponent<AIBasic>().PlayTurn();
                watch.Stop();
                Debug.Log($"Execution Time: {watch.ElapsedMilliseconds} ms");
            }
        }
    }
}