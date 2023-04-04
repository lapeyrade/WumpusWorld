using UnityEngine;

namespace Agent
{
    public class AgentAction
    {
        private readonly Agent _agent;
        
        public AgentAction(Agent agent)
        {
            _agent = agent;
        }
        
        public void PickUpGold()
        {
            GameManager.Instance.UpdateActionGUI($"Picking up gold");
            _agent.nbGold++;
            GridManager.RemoveFromGrids(_agent.coords, "gold");
            GridManager.AttachGoldToAgent(_agent);
        }

        public void TryShootingArrow()
        {
            ShootIfWumpusRightOfAgent();
            ShootIfWumpusLeftOfAgent();
            ShootIfWumpusUpOfAgent();
            ShootIfWumpusDownOfAgent();
        }

        private void ShootIfWumpusRightOfAgent()
        {
            for (var i = _agent.coords.x; i < GameManager.Instance.gridMax.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, _agent.coords.y].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(i, _agent.coords.y), "right");
            }
        }

        private void ShootIfWumpusLeftOfAgent()
        {
            for (var i = GameManager.Instance.gridMin.x; i < _agent.coords.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, _agent.coords.y].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(i, _agent.coords.y), "left");
            }
        }

        private void ShootIfWumpusUpOfAgent()
        {
            for (var i = _agent.coords.y; i < GameManager.Instance.gridMax.y; i++)
            {
                if (GameManager.Instance.AgentsMap[_agent.coords.x, i].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(_agent.coords.x, i), "up");
            }
        }

        private void ShootIfWumpusDownOfAgent()
        {
            for (var i = GameManager.Instance.gridMin.y; i < _agent.coords.y; i++)
            {
                if (GameManager.Instance.AgentsMap[_agent.coords.x, i].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(_agent.coords.x, i), "down");
            }
        }

        private void ShootWumpus(Vector2Int coordWumpus, string direction)
        {
            GameManager.Instance.UpdateActionGUI($"Shooting arrow {direction}");
            _agent.nbArrow--;
            GridManager.RemoveFromGrids(coordWumpus, "wumpus");
            GridManager.RemoveFromGrids(coordWumpus, "danger");
            GridManager.AddToGrids(coordWumpus, "wumpusdead");
            GridManager.AddToGrids(coordWumpus, "safe");
        }
    }
}