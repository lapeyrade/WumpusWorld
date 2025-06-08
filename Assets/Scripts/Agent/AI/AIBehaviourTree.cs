using CleverCrow.Fluid.BTs.Tasks;
using CleverCrow.Fluid.BTs.Trees;
using Ontology;
using UnityEngine;

namespace Agent.AI
{
    public class AIBehaviorTree : AIBasic
    {
        [SerializeField]
        private BehaviorTree tree;

        private void Start()
        {
            // Build behavior tree with the following structure:
            tree = new BehaviorTreeBuilder(gameObject)
                .Sequence("Execute Action")
                    // 1. Generate Objectives based on personality and environment
                    .Selector("Generate Objective")
                        .Sequence("Objective: Safety")
                            .Condition("Coward Personality", () => _agent.GetPersonality<Coward>())
                            .Condition("Dangerous Element", () => _agentObjective.ExistElementNearCells<IDangerous>())
                            .Do("Add Safety", () =>
                            {
                                _agent.SetObjective<Safety>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Fight")
                            .Condition("Brave Personality", () => _agent.GetPersonality<Brave>())
                            .Condition("Monster", () => _agentObjective.ExistElementNearCells<Monster>())
                            .Do("Add Fight", () =>
                            {
                                _agent.SetObjective<Fight>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Wealth")
                            .Condition("Cupid Personality", () => _agent.GetPersonality<Cupid>())
                            .Condition("Valuable Item", () => _agentObjective.ExistElementCell<ValuableItem>())
                            .Do("Add Wealth", () =>
                            {
                                _agent.SetObjective<Wealth>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Abstinence")
                            .Condition("Ascetic Personality", () => _agent.GetPersonality<Ascetic>())
                            .Condition("Item Element", () => _agentObjective.ExistElementCell<Item>())
                            .Do("Add Abstinence", () =>
                            {
                                _agent.SetObjective<Abstinence>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Unconstrained")
                            .Condition("Any Personality", () => _agent.GetPersonality<Personality>())
                            .Condition("Obstacle Element", () => _agentObjective.ExistElementCell<Obstacle>())
                            .Do("Add Unconstrained", () =>
                            {
                                _agent.SetObjective<Unconstrained>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Objective: Explore")
                            .Condition("Any Personality", () => _agent.GetPersonality<Personality>())
                            .Condition("Start, Safe or Visited Cell", () => _agentObjective.ExistTypeCell<SafeCell>() ||
                                _agentObjective.ExistTypeCell<VisitedCell>() || _agentObjective.ExistTypeCell<StartCell>())
                            .Do("Add Explore", () =>
                            {
                                _agent.SetObjective<Explore>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                    .End()

                    // 2. Generate Actions based on objectives and personality
                    .Selector("Generate Action")
                        .Sequence("Action: MoveBack")
                            .Condition("Safety Objective", () => _agent.GetObjective<Safety>())
                            .Condition("Personality Coward", () => _agent.GetPersonality<Coward>())
                            .Do("Add MoveBack", () =>
                            {
                                _agent.SetAction<MoveBack>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Attack")
                            .Condition("Safety Objective", () => _agent.GetObjective<Safety>())
                            .Condition("Personality Brave", () => _agent.GetPersonality<Brave>())
                            .Do("Add Attack", () =>
                            {
                                _agent.SetAction<Attack>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Attack")
                            .Condition("Fight Objective", () => _agent.GetObjective<Fight>())
                            .Condition("Personality Brave", () => _agent.GetPersonality<Brave>())
                            .Do("Add Attack", () =>
                            {
                                _agent.SetAction<Attack>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: PickUp")
                            .Condition("Wealth Objective", () => _agent.GetObjective<Wealth>())
                            .Condition("Personality Cupid", () => _agent.GetPersonality<Cupid>())
                            .Do("Add PickUp", () =>
                            {
                                _agent.SetAction<PickUp>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Discard")
                            .Condition("Abstinence Objective", () => _agent.GetObjective<Abstinence>())
                            .Condition("Personality Ascetic", () => _agent.GetPersonality<Ascetic>())
                            .Do("Add Discard", () =>
                            {
                                _agent.SetAction<Discard>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: BumpWall")
                            .Condition("Unconstrained Objective", () => _agent.GetObjective<Unconstrained>())
                            .Condition("Any Personality", () => _agent.GetPersonality<Personality>())
                            .Do("Add BumpWall", () =>
                            {
                                _agent.SetAction<BumpWall>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                        .Sequence("Action: Move")
                            .Condition("Explore Objective", () => _agent.GetObjective<Explore>())
                            .Condition("Any Personality", () => _agent.GetPersonality<Personality>())
                            .Do("Add Move", () =>
                            {
                                _agent.SetAction<Move>(true);
                                return TaskStatus.Success;
                            })
                        .End()
                    .End()

                    // 3. Evaluate and Execute Actions
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

                    // 4. Update Environment Knowledge
                    .Do("Sense Cell", () =>
                    {
                        _agentSense.SenseCell();
                        return TaskStatus.Success;
                    })

                    // 5. Cleanup for Next Turn
                    .Do("Remove Previous Action", () =>
                    {
                        // Reset all objectives and actions to false after executing actions
                        _agent.ResetObjectives();
                        _agent.ResetActions();
                        return TaskStatus.Success;
                    })
                .End()
                .Build();
        }

        // Update the behavior tree every frame
        public override void PlayTurn() => tree.Tick();
    }
}