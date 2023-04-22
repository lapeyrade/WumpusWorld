using Prolog;

namespace Agent.AI
{
    public class AIProlog : AIBasic 
    {
        public override void FirstTurn()
        {
            GetComponent<Agent>().MoveCell();
            GetComponent<Agent>().SenseCell();
            GameManager.Instance.GetComponent<PrologInterface>().UpdateKb();
            GetComponent<Agent>().ActionCell();
            
            GetComponent<Agent>().SenseCell();
            GameManager.Instance.GetComponent<PrologInterface>().UpdateKb();
        }
        
        public override void PlayTurn()
        {
            var results = GameManager.Instance.GetComponent<PrologInterface>().QueryKb(GetComponent<Agent>().name);
            
            if (results.Contains("attack"))
                GetComponent<Action>().TryShootingArrow();
            else if (results.Contains("pick")) 
                GetComponent<Action>().PickUpGold();
            else if (results.Contains("drop"))
                return;
            else if (results.Contains("bump_wall"))
                GetComponent<Move>().BumpWall();
            else if (results.Contains("move_back"))
                GetComponent<Move>().MoveAgent(GetComponent<Move>().MoveBack());
            else if (results.Contains("move"))
                GetComponent<Agent>().MoveCell();
            
            GetComponent<Agent>().SenseCell();
            GameManager.Instance.GetComponent<PrologInterface>().UpdateKb();
        }
    }
}