using System.Linq;
using CleverCrow.Fluid.BTs.Tasks;
using CleverCrow.Fluid.BTs.Trees;
using Ontology;
using TMPro;
using UnityEngine;

namespace Agent.AI
{
    public class AIBehaviourTree : AIBasic
    {
        [SerializeField]
        private BehaviorTree tree;

        private void Awake() {
            var agentObjective = GetComponent<AgentObjective>();
            var agent = GetComponent<Agent>();
            
            tree = new BehaviorTreeBuilder(gameObject)
                .Sequence("Execute Action")
                    .Selector("Generate Objective")
                        .Sequence("Generate Wealth")
                            .Condition("Cupid Personality", () => agentObjective.ExistPersonality<Cupid>())
                            .Condition("Valuable Item", () => agentObjective.ExistElementCell<ValuableItem>())
                            .Do("Add Wealth", () =>
                            {
                                gameObject.AddComponent<Wealth>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Wealth")
                            .Condition("Brave Personality", () => agentObjective.ExistPersonality<Brave>())
                            .Condition("Monster", () => agentObjective.ExistElementNearCells<Monster>())
                            .Do("Add Fight", () =>
                            {
                                gameObject.AddComponent<Fight>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Safety")
                            .Condition("Coward Personality", () => agentObjective.ExistPersonality<Coward>())
                            .Condition("Dangerous Element", () => agentObjective.ExistElementNearCells<IDangerous>())
                            .Do("Add Safety", () =>
                            {
                                gameObject.AddComponent<Safety>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Abstinence")
                            .Condition("Ascetic Personality", () => agentObjective.ExistPersonality<Ascetic>())
                            .Condition("Item Element", () => agentObjective.ExistElementCell<Item>())
                            .Do("Add Abstinence", () =>
                            {
                                gameObject.AddComponent<Abstinence>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Unconstrained")
                            .Condition("Any Personality", () => agentObjective.ExistPersonality<Personality>())
                            .Condition("Obstacle Element", () => agentObjective.ExistElementCell<Obstacle>())
                            .Do("Add Unconstrained", () =>
                            {
                                gameObject.AddComponent<Unconstrained>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Explore")
                            .Condition("Any Personality", () => agentObjective.ExistPersonality<Personality>())
                            .Condition("Start, Safe or Visited Cell", () => agentObjective.ExistTypeCell<SafeCell>() ||
                                  agentObjective.ExistTypeCell<VisitedCell>() ||
                                  agentObjective.ExistTypeCell<StartCell>())
                            .Do("Add Explore", () =>
                            {
                                gameObject.AddComponent<Explore>();
                                return TaskStatus.Success;
                            })
                        .End()
                    .End()
                    .Selector("Generate Action")
                        .Sequence("Action: PickUp")
                            .Condition("Wealth Objective", () => agent.GetComponent<Wealth>())
                            .Condition("Personality Cupid", () => agent.GetComponent<Cupid>())
                            .Do("Add PickUp", () =>
                            {
                                gameObject.AddComponent<PickUp>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Drop")
                            .Condition("Abstinence Objective", () => agent.GetComponent<Abstinence>())
                            .Condition("Personality Ascetic", () => agent.GetComponent<Ascetic>())
                            .Do("Add Drop", () =>
                            {
                                gameObject.AddComponent<Drop>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: MoveBack")
                            .Condition("Safety Objective", () => agent.GetComponent<Safety>())
                            .Condition("Personality Coward", () => agent.GetComponent<Coward>())
                            .Do("Add MoveBack", () =>
                            {
                                gameObject.AddComponent<MoveBack>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Attack")
                            .Condition("Safety Objective", () => agent.GetComponent<Safety>())
                            .Condition("Personality Brave", () => agent.GetComponent<Brave>())
                            .Do("Add Attack", () =>
                            {
                                gameObject.AddComponent<Attack>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Attack")
                            .Condition("Fight Objective", () => agent.GetComponent<Fight>())
                            .Condition("Personality Brave", () => agent.GetComponent<Brave>())
                            .Do("Add Attack", () =>
                            {
                                gameObject.AddComponent<Attack>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Move")
                            .Condition("Explore Objective", () => agent.GetComponent<Explore>())
                            .Condition("Any Personality", () => agent.GetComponent<Personality>())
                            .Do("Add Move", () =>
                            {
                                gameObject.AddComponent<Move>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: BumpWall")
                            .Condition("Unconstrained Objective", () => agent.GetComponent<Unconstrained>())
                            .Condition("Any Personality", () => agent.GetComponent<Personality>())
                            .Do("Add BumpWall", () =>
                            {
                                gameObject.AddComponent<BumpWall>();
                                return TaskStatus.Success;
                            })
                        .End()
                    .End()
                    .Do("Generate Utility", () =>
                    {
                        GetComponent<AgentAction>().GenerateUtility();
                        return TaskStatus.Success;
                    })
                    .Do("Execute Highest Utility Action", () =>
                        {
                            agent.GetComponents<Component>()
                            .Where(c => c is Action)
                            .OrderByDescending(c => c.GetComponent<Action>().Utility)
                            .First().GetComponent<Action>().Act();
                            
                            var highestUtilityComponent = agent.GetComponents<Component>()
                                .Where(c => c is Action)
                                .OrderByDescending(c => c.GetComponent<Action>().Utility)
                                .First().GetComponent<Action>();
                            
                            GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>().captionText.text =
                                $"{agent.name} chose the action {highestUtilityComponent} with a utility of" +
                                $" {highestUtilityComponent.GetComponent<Action>().Utility}.";
                            
                            GetComponents<Component>().Where(c => c is Objective or Move or Action).ToList().ForEach(Destroy);

                            return TaskStatus.Success;
                        })
                    .Do("Sense Cell", () =>
                    {
                        GetComponent<AgentSense>().SenseCell();
                        return TaskStatus.Success;
                    })
                .End()
                .Build();
        }

        public override void PlayTurn () => tree.Tick(); // Update the tree every frame
    }
}