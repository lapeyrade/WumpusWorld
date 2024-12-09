using System.Linq;
using CleverCrow.Fluid.BTs.Tasks;
using CleverCrow.Fluid.BTs.Trees;
using Ontology;
using UnityEngine;
using Agent;

namespace Agent.AI
{
    public class AIBehaviourTree : AIBasic
    {
        [SerializeField]
        private BehaviorTree tree;
        private AgentObjective _agentObjective;
        private Agent _agent;
        private AgentAction _agentAction;
        private AgentSense _agentSense;

        private void Start()
        {
            _agentObjective = GetComponent<AgentObjective>();
            _agent = GetComponent<Agent>();
            _agentAction = GetComponent<AgentAction>();
            _agentSense = GetComponent<AgentSense>();

            tree = new BehaviorTreeBuilder(gameObject)
                .Sequence("Execute Action")
                    .Selector("Generate Objective")
                        .Sequence("Generate Wealth")
                            .Condition("Cupid Personality", () => _agentObjective.ExistPersonality<Cupid>())
                            .Condition("Valuable Item", () => _agentObjective.ExistElementCell<ValuableItem>())
                            .Do("Add Wealth", () =>
                            {
                                gameObject.AddComponent<Wealth>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Wealth")
                            .Condition("Brave Personality", () => _agentObjective.ExistPersonality<Brave>())
                            .Condition("Monster", () => _agentObjective.ExistElementNearCells<Monster>())
                            .Do("Add Fight", () =>
                            {
                                gameObject.AddComponent<Fight>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Safety")
                            .Condition("Coward Personality", () => _agentObjective.ExistPersonality<Coward>())
                            .Condition("Dangerous Element", () => _agentObjective.ExistElementNearCells<IDangerous>())
                            .Do("Add Safety", () =>
                            {
                                gameObject.AddComponent<Safety>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Abstinence")
                            .Condition("Ascetic Personality", () => _agentObjective.ExistPersonality<Ascetic>())
                            .Condition("Item Element", () => _agentObjective.ExistElementCell<Item>())
                            .Do("Add Abstinence", () =>
                            {
                                gameObject.AddComponent<Abstinence>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Unconstrained")
                            .Condition("Any Personality", () => _agentObjective.ExistPersonality<Personality>())
                            .Condition("Obstacle Element", () => _agentObjective.ExistElementCell<Obstacle>())
                            .Do("Add Unconstrained", () =>
                            {
                                gameObject.AddComponent<Unconstrained>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Explore")
                            .Condition("Any Personality", () => _agentObjective.ExistPersonality<Personality>())
                            .Condition("Start, Safe or Visited Cell", () => _agentObjective.ExistTypeCell<SafeCell>() ||
                                  _agentObjective.ExistTypeCell<VisitedCell>() || _agentObjective.ExistTypeCell<StartCell>())
                            .Do("Add Explore", () =>
                            {
                                gameObject.AddComponent<Explore>();
                                return TaskStatus.Success;
                            })
                        .End()
                    .End()
                    .Selector("Generate Action")
                        .Sequence("Action: PickUp")
                            .Condition("Wealth Objective", () => _agent.GetComponent<Wealth>())
                            .Condition("Personality Cupid", () => _agent.GetComponent<Cupid>())
                            .Do("Add PickUp", () =>
                            {
                                gameObject.AddComponent<PickUp>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Discard")
                            .Condition("Abstinence Objective", () => _agent.GetComponent<Abstinence>())
                            .Condition("Personality Ascetic", () => _agent.GetComponent<Ascetic>())
                            .Do("Add Discard", () =>
                            {
                                gameObject.AddComponent<Discard>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: MoveBack")
                            .Condition("Safety Objective", () => _agent.GetComponent<Safety>())
                            .Condition("Personality Coward", () => _agent.GetComponent<Coward>())
                            .Do("Add MoveBack", () =>
                            {
                                gameObject.AddComponent<MoveBack>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Attack")
                            .Condition("Safety Objective", () => _agent.GetComponent<Safety>())
                            .Condition("Personality Brave", () => _agent.GetComponent<Brave>())
                            .Do("Add Attack", () =>
                            {
                                gameObject.AddComponent<Attack>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Attack")
                            .Condition("Fight Objective", () => _agent.GetComponent<Fight>())
                            .Condition("Personality Brave", () => _agent.GetComponent<Brave>())
                            .Do("Add Attack", () =>
                            {
                                gameObject.AddComponent<Attack>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Move")
                            .Condition("Explore Objective", () => _agent.GetComponent<Explore>())
                            .Condition("Any Personality", () => _agent.GetComponent<Personality>())
                            .Do("Add Move", () =>
                            {
                                gameObject.AddComponent<Move>();
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: BumpWall")
                            .Condition("Unconstrained Objective", () => _agent.GetComponent<Unconstrained>())
                            .Condition("Any Personality", () => _agent.GetComponent<Personality>())
                            .Do("Add BumpWall", () =>
                            {
                                gameObject.AddComponent<BumpWall>();
                                return TaskStatus.Success;
                            })
                        .End()
                    .End()
                    .Do("Generate Utility", () =>
                    {
                        _agentAction.GenerateUtility();
                        return TaskStatus.Success;
                    })
                    .Do("Execute Highest Utility Action", () =>
                        {
                            _agentAction.ExecuteHighestUtility();
                            return TaskStatus.Success;
                        })
                    .Do("Sense Cell", () =>
                    {
                        _agentSense.SenseCell();
                        return TaskStatus.Success;
                    })
                    .Do("Remove Previous Action", () =>
                    {
                        GetComponents<Component>().Where(c => c is Objective or Move or Action).ToList().ForEach(Destroy);
                        return TaskStatus.Success;
                    })
                .End()
                .Build();
        }

        public override void PlayTurn() => tree.Tick(); // Update the tree every frame
    }
}