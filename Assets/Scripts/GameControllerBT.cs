using UnityEngine;
using CleverCrow.Fluid.BTs.Tasks;
using CleverCrow.Fluid.BTs.Trees;

public class GameControllerBt : MonoBehaviour {
    [SerializeField]
    private BehaviorTree tree;
    private bool _isInitialTurn = true;

    private void Awake() {
        tree = new BehaviorTreeBuilder(gameObject)
            .Selector("Play game")
                .Sequence("Initial turn")
                    .Condition("Is initial turn", () => _isInitialTurn)
                    .Do("Play initial turn", () =>
                    {
                        PlayTurn();
                        _isInitialTurn = false;
                        return TaskStatus.Success;
                    })
                .End()
                .Sequence("No action")
                    .Condition("No active command", () => !Input.anyKeyDown)
                    .Condition("Mode is not auto", () => GameManager.Instance.isModeAuto)
                    .Condition("Game is over", () => GameManager.Instance.isGameOver)
                    .Do("Wait for next action", () => TaskStatus.Success)
                .End()
                .Sequence("Exit game")
                    .Condition("Escape key pressed", () => Input.GetKeyDown(KeyCode.Escape))
                    .Do("Setting game over", () =>
                    {
                        GameManager.Instance.SetGameOver("Exit Game!", true);
                        return TaskStatus.Success;
                    })
                .End()
                .Sequence("Take game screenshot")
                    .Condition("Backspace key pressed", () => Input.GetKeyDown(KeyCode.Backspace))
                    .Do("Capture screenshot", () =>
                    {
                        ScreenCapture.CaptureScreenshot("Screenshots/screenshot " +
                                                        System.DateTime.Now.ToString("MM-dd-yy (HH-mm-ss)") + ".png");
                        Debug.Log("Screenshot saved!");
                        return TaskStatus.Success;
                    })
                .End()
                .Selector("Play turn")
                    .Condition("Return key pressed", () => Input.GetKeyDown("return"))
                    .Condition("Space key pressed", () => Input.GetKeyDown("space"))
                    .Condition("Right key pressed", () => Input.GetKeyDown("right"))
                    .Condition("Left key pressed", () => Input.GetKeyDown("left"))
                    .Condition("Up key pressed", () => Input.GetKeyDown("up"))
                    .Condition("Down key pressed", () => Input.GetKeyDown("down"))
                    .Condition("Is mode auto", () => !GameManager.Instance.isModeAuto)
                    .Do ("Play turn", () =>
                    {
                        PlayTurn();
                        return TaskStatus.Success;
                    })
                .End()
            .End()
            .Build();
    }

    private void Update () {
        // Update our tree every frame
        tree.Tick();
    }

    private void PlayTurn()
    {
        Debug.Log("Playing turn");
        foreach (Human agent in GameManager.Instance.agents)
        {
            agent.PlayTurn();
        }
    }
}