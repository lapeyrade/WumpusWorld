using UnityEngine;

namespace Ontology
{
    public class Element : MonoBehaviour { }
    public class Being : Element { }
    public class Object : Element  { }
    public class Environment : Element { }
    public class Breeze : Environment { }
    public class Stench : Environment { }
    public class Trap : Object { }
    public class Item : Object { }
    public class Obstacle : Object { }
    public class Wall : Obstacle { }
    public class ValuableItem : Item { }
    public class CommonItem : Item { }
    public class UnvaluableItem : Item { }
    public class Animal : Being { }
    public class Plant : Being { }
    public class Gold : ValuableItem { }
    public class Rock : UnvaluableItem { }
    public class Weapon : Item { }
    public class Bow : Weapon { }
    public class Sword : Weapon { }
    public class Monster : Animal { }
    public class Human : Animal { }
    public class Dog : Animal { }
    public class Dragon : Monster { }
    public class Wumpus : Monster { }
    public class Pit : Trap { }
}