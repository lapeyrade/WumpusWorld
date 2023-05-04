using Prolog;

namespace Agent.AI
{
    public class AIProlog : AIBasic 
    {
        public override void FirstTurn()
        {
            GetComponent<AgentMove>().MoveCell();
            GetComponent<AgentSense>().SenseCell();
            GameManager.Instance.GetComponent<PrologInterface>().UpdateKb();
        }
        
        public override void PlayTurn()
        {
            switch (GameManager.Instance.GetComponent<PrologInterface>().QueryKb(GetComponent<Agent>().name))
            {
                case "attack" or "shoot" or "shootarrow":
                    GetComponent<AgentAction>().TryShootingArrow();
                    break;
                case "pickup":
                    GetComponent<AgentAction>().PickUpGold();
                    break;
                case "drop":
                    return;
                case "bumpwall":
                    GetComponent<AgentMove>().BumpWall();
                    break;
                case "moveback":
                    GetComponent<AgentMove>().MoveAgent(GetComponent<AgentMove>().MoveBack());
                    break;
                case "move":
                    GetComponent<AgentMove>().MoveCell();
                    break;
            }
            
            GetComponent<AgentSense>().SenseCell();
            GameManager.Instance.GetComponent<PrologInterface>().UpdateKb();
        }
    }
}