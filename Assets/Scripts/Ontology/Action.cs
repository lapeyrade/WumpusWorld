using Agent;
using UnityEngine;

namespace Ontology
{
    public class Action : MonoBehaviour
    {
        public int Utility { get; set; } = 1;
        public virtual void Act() { }
    }

    public class Interact : Action { }

    public class Attack : Action { public override void Act() => GetComponent<AgentAction>().TryShootingArrow(); }

    public class BumpWall : Action { public override void Act() => GetComponent<AgentMove>().BumpWall(); }

    public class Move : Action { public override void Act() => GetComponent<AgentMove>().MoveCell(); }

    public class MoveBack : Action { public override void Act() => GetComponent<AgentMove>().MoveAgent(GetComponent<AgentMove>().MoveBack()); }

    public class PickUp : Interact { public override void Act() => GetComponent<AgentAction>().PickUpGold(); }

    public class Discard : Interact { public override void Act() => GetComponent<AgentAction>().Discard(); }

    public class Shoot : Attack { }

    public class ShootArrow : Shoot { }
}