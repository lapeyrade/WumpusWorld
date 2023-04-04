namespace Agent
{
    public class AgentAI
    {
        private readonly Agent _agent;
    
        public AgentAI(Agent agent)
        {
            _agent = agent;
        }

        public void PlayTurn()
        {
            _agent.MoveCell();
            _agent.SenseCell();
            _agent.ActionCell();
        }
    }
}
