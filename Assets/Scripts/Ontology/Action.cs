using UnityEngine;

namespace Ontology
{
    public class Action : MonoBehaviour { }
    public class Interact : Action { }
    public class Attack : Action { }
    public class BumpWall : Action { }
    public class Move : Action { }
    public class MoveBack : Action { }
    
    public class PickUp : Interact { }
    public class Drop : Interact { }
    public class Shoot : Attack { }
    public class ShootArrow : Shoot { }
}