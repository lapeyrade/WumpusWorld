using UnityEngine;

namespace Ontology
{
    public class Cell : MonoBehaviour { }
    public class StartCell : Cell { }
    public class SafeCell : Cell { }
    public class VisitedCell : Cell { }
    public class UnknownCell : Cell { }
    public class DangerousCell : Cell { }
    public class DeadWumpus : SafeCell { }
}