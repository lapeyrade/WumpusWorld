using UnityEngine;

namespace Agent
{
    public class AgentAction : MonoBehaviour
    {
        public void PickUpGold()
        {
            GameManager.UpdateActionGUI($"Picking up gold");
            GetComponent<Agent>().nbGold++;
            GridManager.RemoveFromGrids(GetComponent<Agent>().coords, "Gold");
            GridManager.AttachGoldToAgent(GetComponent<Agent>());
        }

        public void TryShootingArrow()
        {
            if (GetComponent<Agent>().nbArrow < 1) return;
            
            ShootIfWumpusRightOfAgent();
            ShootIfWumpusLeftOfAgent();
            ShootIfWumpusUpOfAgent();
            ShootIfWumpusDownOfAgent();
        }

        private void ShootIfWumpusRightOfAgent()
        {
            for (var i = GetComponent<Agent>().coords.x; i < GameManager.Instance.gridMax.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, GetComponent<Agent>().coords.y].Exists(e => e.tag is "Wumpus"))
                    ShootWumpus(new Vector2Int(i, GetComponent<Agent>().coords.y), "right");
            }
        }

        private void ShootIfWumpusLeftOfAgent()
        {
            for (var i = GameManager.Instance.gridMin.x; i < GetComponent<Agent>().coords.x; i++)
            {
                if (GameManager.Instance.AgentsMap[i, GetComponent<Agent>().coords.y].Exists(e => e.tag is "Wumpus"))
                    ShootWumpus(new Vector2Int(i, GetComponent<Agent>().coords.y), "left");
            }
        }

        private void ShootIfWumpusUpOfAgent()
        {
            for (var i = GetComponent<Agent>().coords.y; i < GameManager.Instance.gridMax.y; i++)
            {
                if (GameManager.Instance.AgentsMap[GetComponent<Agent>().coords.x, i].Exists(e => e.tag is "Wumpus"))
                    ShootWumpus(new Vector2Int(GetComponent<Agent>().coords.x, i), "up");
            }
        }

        private void ShootIfWumpusDownOfAgent()
        {
            for (var i = GameManager.Instance.gridMin.y; i < GetComponent<Agent>().coords.y; i++)
            {
                if (GameManager.Instance.AgentsMap[GetComponent<Agent>().coords.x, i].Exists(e => e.tag is "Wumpus"))
                    ShootWumpus(new Vector2Int(GetComponent<Agent>().coords.x, i), "down");
            }
        }

        private void ShootWumpus(Vector2Int coordWumpus, string direction)
        {
            GameManager.UpdateActionGUI($"Shooting arrow {direction}");
            GetComponent<Agent>().nbArrow--;
            GridManager.RemoveFromGrids(coordWumpus, "Wumpus");
            GridManager.RemoveFromGrids(coordWumpus, "DangerousCell");
            GridManager.AddToGrids(coordWumpus, "DeadWumpus");
            GridManager.AddToGrids(coordWumpus, "SafeCell");
        }
    }
}