using System.Linq;
using CleverCrow.Fluid.BTs.Tasks;
using CleverCrow.Fluid.BTs.Trees;
using Ontology;
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
                    .Do("Sense Cell", () =>
                    {
                        GetComponent<Agent>().SenseCell();
                        return TaskStatus.Success;
                    })
                    .Do("Generate Objective", () =>
                    {
                        GetComponent<Agent>().GenerateObjective();
                        return TaskStatus.Success;
                    })
                    .Do("Generate Action", () =>
                    {
                        GetComponent<Agent>().GenerateAction();
                        return TaskStatus.Success;
                    })
                    .Selector("Action or Move")
                        .Sequence("Attack")
                            .Condition("Attack Generated", () => GetComponent<Agent>().GetComponent<Attack>())
                            .Do("Shoot Monster", () =>
                            {
                                GetComponent<AgentAction>().TryShootingArrow();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("PickUp")
                            .Condition("PickUp Generated", () => GetComponent<Agent>().GetComponent<PickUp>())
                            .Do("PickUp ValuableItem", () =>
                            {
                                GetComponent<AgentAction>().PickUpGold();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Drop")
                            .Condition("Drop Generated", () => GetComponent<Agent>().GetComponent<Drop>())
                            .Do("Drop Item", () =>
                            {
                                Debug.Log("Droping Item!");
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Bump")
                            .Condition("BumpWall Generated", () => GetComponent<Agent>().GetComponent<BumpWall>())
                            .Do("Bump Wall", () =>
                            {
                                GetComponent<AgentMove>().BumpWall();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("MoveBack")
                            .Condition("MoveBack Generated", () => GetComponent<Agent>().GetComponent<MoveBack>())
                            .Do("Move Back", () =>
                            {
                                GetComponent<AgentMove>().MoveAgent(GetComponent<AgentMove>().MoveBack());
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Move")
                            .Condition("Move Action Generated", () => GetComponent<Agent>().GetComponent<Move>())
                            .Do("Move", () =>
                            {
                                GetComponent<Agent>().MoveCell();
                                return TaskStatus.Success;
                            })
                        .End()
                    .End()
                    .Do("Sense Cell", () =>
                    {
                        GetComponent<Agent>().SenseCell();
                        return TaskStatus.Success;
                    })
                    .Do("Resetting Objectives and Actions", () =>
                    {
                        foreach (var component in GetComponents<Component>().Where(c => c is Objective or Move or Action))
                        {
                            Debug.Log(component);
                            Destroy(component);
                        }
                        return TaskStatus.Success;
                    })
                
                .End()
                .Build();
        }

        public override void PlayTurn ()
        {
            tree.Tick();  // Update the tree every frame
        }
    }
}