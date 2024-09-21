using UnityEngine;

namespace Ontology
{
    public class Objective : MonoBehaviour { public virtual bool IsPreconditionMet() => true; }

    public class Success : Objective { }
    public class Healthiness : Objective { }

    public class Unconstrained : Objective { }

    public class Explore : Objective { }

    public class Wealth : Success { }
    public class Fight : Success { }
    public class Abstinence : Healthiness { }
    public class Safety : Healthiness { }
}