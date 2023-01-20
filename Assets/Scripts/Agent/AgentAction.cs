using UnityEngine;

namespace Agent
{
    public class AgentAction : MonoBehaviour
    {
        private Agent _agent;

        private void Start()
        {
            _agent = gameObject.GetComponent<Agent>();
        }

        public void PickUpGold()
        {
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
            for (int i = _agent.coords.x; i < GameManager.Instance.gridMax.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, _agent.coords.y].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(i, _agent.coords.y));
            }
        }

        private void ShootIfWumpusLeftOfAgent()
        {
            for (int i = GameManager.Instance.gridMin.x; i < _agent.coords.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, _agent.coords.y].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(i, _agent.coords.y));
            }
        }

        private void ShootIfWumpusUpOfAgent()
        {
            for (int i = _agent.coords.y; i < GameManager.Instance.gridMax.y; i++)
            {
                if (GameManager.Instance.AgentsMap[_agent.coords.x, i].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(_agent.coords.x, i));
            }
        }

        private void ShootIfWumpusDownOfAgent()
        {
            for (int i = GameManager.Instance.gridMin.y; i < _agent.coords.y; i++)
            {
                if (GameManager.Instance.AgentsMap[_agent.coords.x, i].Exists(e => e.tag is "wumpus"))
                    ShootWumpus(new Vector2Int(_agent.coords.x, i));
            }
        }

        private void ShootWumpus(Vector2Int coordWumpus)
        {
            _agent.nbArrow--;
            GridManager.RemoveFromGrids(coordWumpus, "wumpus");
            GridManager.RemoveFromGrids(coordWumpus, "danger");
            GridManager.AddToGrids(coordWumpus, "wumpusdead");
            GridManager.AddToGrids(coordWumpus, "safe");
        }
    }
}