using UnityEngine;

namespace Ontology
{
    public class Action : MonoBehaviour { }
    public class Interact : Action { }
    public class Attack : Action { }
    public class Move : Action { }
    public class PickUp : Interact { }
    public class Drop : Interact { }
    public class Shoot : Attack { }
    public class ShootArrow : Shoot { }
}