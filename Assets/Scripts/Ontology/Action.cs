using Agent;
using UnityEngine;

namespace Ontology
{
    public class Action : MonoBehaviour
    {
        public int Utility { get; set; } = 0;
        public virtual void Act() { }
    }

    public class Interact : Action { }

    public class Attack : Action 
    { 
        private AgentAction _agentAction;

        private void Awake() => _agentAction = GetComponent<AgentAction>();

        public override void Act() => _agentAction.TryShootingArrow(); 
    }

    public class BumpWall : Action 
    { 
        private AgentMove _agentMove;

        private void Awake() => _agentMove = GetComponent<AgentMove>();

        public override void Act() => _agentMove.BumpWall(); 
    }

    public class Move : Action 
    { 
        private AgentMove _agentMove;

        private void Awake() => _agentMove = GetComponent<AgentMove>();

        public override void Act() => _agentMove.MoveCell(); 
    }

    public class MoveBack : Action 
    { 
        private AgentMove _agentMove;

        private void Awake() => _agentMove = GetComponent<AgentMove>();

        public override void Act() => _agentMove.MoveAgent(_agentMove.MoveBack()); 
    }

    public class PickUp : Interact 
    { 
        private AgentAction _agentAction;

        private void Awake() => _agentAction = GetComponent<AgentAction>();

        public override void Act() => _agentAction.PickUpGold(); 
    }

    public class Discard : Interact 
    { 
        private AgentAction _agentAction;

        private void Awake() => _agentAction = GetComponent<AgentAction>();

        public override void Act() => _agentAction.Discard(); 
    }

    public class Shoot : Attack { }

    public class ShootArrow : Shoot { }
}