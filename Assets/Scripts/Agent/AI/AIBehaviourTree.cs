using CleverCrow.Fluid.BTs.Tasks;
using CleverCrow.Fluid.BTs.Trees;
using UnityEngine;

namespace Agent.AI
{
    public class AIBehaviourTree : AIBasic
    {
        [SerializeField]
        private BehaviorTree tree;
        
        private void Awake() {
            tree = new BehaviorTreeBuilder(gameObject)
                .Sequence("Play Turn")
                .Do("Move Cell", () =>
                        {
                            GetComponent<Agent>().MoveCell();
                            return TaskStatus.Success;
                        }).Do("Sense Cell", () =>
                        {
                            GetComponent<Agent>().SenseCell();
                            return TaskStatus.Success;
                        })
                        .Do("Generate Objective", () =>
                        {
                            GenerateObjective();
                            return TaskStatus.Success;
                        })
                        .Do("Action Cell", () =>
                        {
                            GetComponent<Agent>().ActionCell();
                            return TaskStatus.Success;
                        })
                .End()
                .Build();
        }

        private void GenerateObjective()
        {
            GetComponent<Agent>().objectives.Clear();
            
            if (GetComponent<Agent>().personalities.Contains(GameManager.Personalities.Cupid) &&
                GameManager.Instance.AgentsMap[GetComponent<Agent>().coords.x, GetComponent<Agent>().coords.y]
                    .Exists(e => e.tag is "valuable_item"))
            {
                Debug.Log("Objective: Wealthy");
                GetComponent<Agent>().objectives.Add(Agent.Objectives.Wealth);
            }
            else if (GetComponent<Agent>().personalities.Contains(GameManager.Personalities.Brave) &&
                     GameManager.Instance.AgentsMap[GetComponent<Agent>().coords.x, GetComponent<Agent>().coords.y]
                         .Exists(e => e.tag is "wall"))
            {
                Debug.Log("Objective: Unstuck");
                GetComponent<Agent>().objectives.Add(Agent.Objectives.Unconstrained);
            }
            else if (Sense.DangerInRightCell(GetComponent<Agent>().coords, "wumpus") ||
                     Sense.DangerInLeftCell(GetComponent<Agent>().coords, "wumpus") ||
                     Sense.DangerInUpCell(GetComponent<Agent>().coords, "wumpus") ||
                     Sense.DangerInDownCell(GetComponent<Agent>().coords, "wumpus"))
            {
                Debug.Log("Objective: Battle");
                GetComponent<Agent>().objectives.Add(Agent.Objectives.Fight);
                Debug.Log("Objective: Safety");
                GetComponent<Agent>().objectives.Add(Agent.Objectives.Safety);
            }
            else if (Sense.DangerInRightCell(GetComponent<Agent>().coords, "pit") ||
                     Sense.DangerInLeftCell(GetComponent<Agent>().coords, "pit") ||
                     Sense.DangerInUpCell(GetComponent<Agent>().coords, "pit") ||
                     Sense.DangerInDownCell(GetComponent<Agent>().coords, "pit"))
            {
                Debug.Log("Objective: Safety");
                GetComponent<Agent>().objectives.Add(Agent.Objectives.Safety);
            }
        }

        public override void PlayTurn ()
        {
            tree.Tick();  // Update the tree every frame
        }
    }
}