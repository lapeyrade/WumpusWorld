using Ontology;
using UnityEngine;

namespace Agent
{
    public class AgentAction : MonoBehaviour
    {
        private Agent Agent => GetComponent<Agent>();
        private Vector2Int Coords => Agent.coords;
        
        public void ActionCell()
        {
            if(GameManager.Instance.isGameOver) return;
            
            if (GameManager.Instance.AgentsMap[Coords.x, Coords.y].Exists(e => e.tag is "Wall"))
                GetComponent<AgentMove>().BumpWall();
            
            if (GameManager.Instance.AgentsMap[Coords.x, Coords.y].Exists(e => e.tag is "Gold"))
                GetComponent<AgentAction>().PickUpGold();
            
            GetComponent<AgentAction>().TryShootingArrow();
        }
        
        public void GenerateAction()
        {
            if (GetComponent<Wealth>() && GetComponent<Cupid>())
                gameObject.AddComponent<PickUp>();
            if (GetComponent<Abstinence>() && GetComponent<Ascetic>())
                gameObject.AddComponent<Drop>();
            if (GetComponent<Safety>() && GetComponent<Coward>())
                gameObject.AddComponent<MoveBack>();
            if (GetComponent<Safety>() && GetComponent<Brave>())
                gameObject.AddComponent<Attack>();
            if (GetComponent<Fight>() && GetComponent<Brave>())
                gameObject.AddComponent<Attack>();
            if (GetComponent<Explore>() && GetComponent<Personality>())
                gameObject.AddComponent<Move>();
            if (GetComponent<Unconstrained>() && GetComponent<Personality>())
                gameObject.AddComponent<BumpWall>();
        }

        public void GenerateUtility()
        {
            if (GetComponent<Cupid>() && GetComponent<Interact>())
                gameObject.GetComponent<Interact>().Utility = 5;
            if (GetComponent<Ascetic>() && GetComponent<Interact>())
                gameObject.GetComponent<Interact>().Utility = 3;
            if (GetComponent<Coward>() && GetComponent<MoveBack>())
                gameObject.GetComponent<MoveBack>().Utility = 10;
            if (GetComponent<Brave>() && GetComponent<Attack>())
                gameObject.GetComponent<Attack>().Utility = 9;
            if (GetComponent<Personality>() && GetComponent<Move>())
                gameObject.GetComponent<Move>().Utility = 1;
            if (GetComponent<Personality>() && GetComponent<BumpWall>())
                gameObject.GetComponent<BumpWall>().Utility = 2;
        }
        
        public void PickUpGold()
        {
            GameManager.UpdateActionGUI($"Picking up gold");
            Agent.nbGold++;
            GridManager.RemoveFromGrids(Coords, "Gold");
            GridManager.AttachGoldToAgent(GetComponent<Agent>());
        }

        public void TryShootingArrow()
        {
            if (Agent.nbArrow < 1) return;
            
            ShootIfWumpusRightOfAgent();
            ShootIfWumpusLeftOfAgent();
            ShootIfWumpusUpOfAgent();
            ShootIfWumpusDownOfAgent();
        }

        private void ShootIfWumpusRightOfAgent()
        {
            for (var i = Coords.x; i < GameManager.Instance.gridMax.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, Coords.y].Exists(e => e.tag is "Wumpus"))
                    ShootWumpus(new Vector2Int(i, Coords.y), "right");
            }
        }

        private void ShootIfWumpusLeftOfAgent()
        {
            for (var i = GameManager.Instance.gridMin.x; i < Coords.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, Coords.y].Exists(e => e.tag is "Wumpus"))
                    ShootWumpus(new Vector2Int(i, Coords.y), "left");
            }
        }

        private void ShootIfWumpusUpOfAgent()
        {
            for (var i = Coords.y; i < GameManager.Instance.gridMax.y; i++)
            {
                if (GameManager.Instance.AgentsMap[Coords.x, i].Exists(e => e.tag is "Wumpus"))
                    ShootWumpus(new Vector2Int(Coords.x, i), "up");
            }
        }

        private void ShootIfWumpusDownOfAgent()
        {
            for (var i = GameManager.Instance.gridMin.y; i < Coords.y; i++)
            {
                if (GameManager.Instance.AgentsMap[Coords.x, i].Exists(e => e.tag is "Wumpus"))
                    ShootWumpus(new Vector2Int(Coords.x, i), "down");
            }
        }

        private void ShootWumpus(Vector2Int coordWumpus, string direction)
        {
            GameManager.UpdateActionGUI($"Shooting arrow {direction}");
            Agent.nbArrow--;
            GridManager.RemoveFromGrids(coordWumpus, "Wumpus");
            GridManager.RemoveFromGrids(coordWumpus, "DangerousCell");
            GridManager.AddToGrids(coordWumpus, "DeadWumpus");
            GridManager.AddToGrids(coordWumpus, "SafeCell");
        }
    }
}