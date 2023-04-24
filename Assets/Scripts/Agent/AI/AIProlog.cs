using Prolog;
using TMPro;
using UnityEngine;

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
                GetComponent<AgentAction>().TryShootingArrow();
            else if (results.Contains("pickup")) 
                GetComponent<AgentAction>().PickUpGold();
            else if (results.Contains("drop"))
                return;
            else if (results.Contains("bumpwall"))
                GetComponent<AgentMove>().BumpWall();
            else if (results.Contains("moveback"))
                GetComponent<AgentMove>().MoveAgent(GetComponent<AgentMove>().MoveBack());
            else if (results.Contains("move"))
                GetComponent<Agent>().MoveCell();

            GetComponent<Agent>().SenseCell();
            GameManager.Instance.GetComponent<PrologInterface>().UpdateKb();
        }
    }
}