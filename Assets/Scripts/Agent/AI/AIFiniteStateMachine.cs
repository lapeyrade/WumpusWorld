using UnityEngine;
using System.Collections.Generic;

namespace Agent.AI
{
    public class AIFiniteStateMachine : AIBasic
    {
        // Define the possible states for the finite state machine
        // States follow a fixed sequence: GenerateObjective -> GenerateAction -> GenerateUtility -> 
        // ExecuteHighestUtilityAction -> SenseCell -> (back to GenerateObjective)
        private enum State
        {
            GenerateObjective,          // Determine agent's goals based on personality and environment
            GenerateAction,            // Generate possible actions based on objectives
            GenerateUtility,           // Calculate utility values for each action
            ExecuteHighestUtilityAction, // Execute the action with highest utility
            SenseCell                   // Update knowledge of environment
        }

        private State _currentState = State.GenerateObjective;
        private readonly Dictionary<State, State> _stateTransitions;
        private bool _turnComplete;

        public AIFiniteStateMachine()
        {
            // Define state transitions in constructor
            // Each state has exactly one next state, forming a cycle
            _stateTransitions = new Dictionary<State, State>
            {
                { State.GenerateObjective, State.GenerateAction },
                { State.GenerateAction, State.GenerateUtility },
                { State.GenerateUtility, State.ExecuteHighestUtilityAction },
                { State.ExecuteHighestUtilityAction, State.SenseCell },
                { State.SenseCell, State.GenerateObjective }
            };
        }

        // The first turn initializes the agent's position and gathers initial environment information
        public override void FirstTurn()
        {
            if (!IsAgentReady()) return;

            _agentMove.MoveCell();    // Move to initial position
            _agentSense.SenseCell();  // Gather initial environment data
        }

        // Execute the finite state machine cycle for one complete turn
        public override void PlayTurn()
        {
            if (!IsAgentReady()) return;

            _turnComplete = false;
            while (!_turnComplete)
            {
                ExecuteCurrentState();
                TransitionToNextState();
            }
        }

        // Execute the logic for the current state
        private void ExecuteCurrentState()
        {
            switch (_currentState)
            {
                case State.GenerateObjective:
                    _agentObjective.GenerateObjective();
                    break;
                case State.GenerateAction:
                    _agentAction.GenerateAction();
                    break;
                case State.GenerateUtility:
                    _agentAction.GenerateUtility();
                    break;
                case State.ExecuteHighestUtilityAction:
                    _agentAction.ExecuteHighestUtility();
                    break;
                case State.SenseCell:
                    _agentSense.SenseCell();
                    CleanupAgentState();
                    _turnComplete = true;  // Mark turn as complete after sensing and cleanup
                    break;
            }
        }

        // Move to the next state based on the transition dictionary
        private void TransitionToNextState()
        {
            if (_stateTransitions.TryGetValue(_currentState, out State nextState))
                _currentState = nextState;
        }

        // Reset objectives and actions for the next turn
        private void CleanupAgentState()
        {
            _agent.ResetObjectives();
            _agent.ResetActions();
        }

        // Verify that all required agent components are available
        private bool IsAgentReady() =>
            _agent != null && _agentAction != null && _agentObjective != null && _agentSense != null;
    }
}