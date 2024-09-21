using UnityEngine;

namespace Ontology
{
    public class Personality : MonoBehaviour { }

    public class Ambitious : Personality { }
    public class Disciplined : Personality { }
    public class Sensitive : Personality { }

    public class Cupid : Ambitious { }
    public class Brave : Ambitious { }
    public class Ascetic : Disciplined { }
    public class Coward : Sensitive { }
}